"""Refresh native Lab 3 hints and replace the legacy solution module reference.

Read-only by default. Preserve all publication states and unrelated item order.
Old Canvas files remain untouched. Upload the new workbook without overwrites.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
from urllib.parse import urlencode
from urllib.request import urlopen
from zipfile import ZipFile
from eco230_canvas.content import _canvas_quarto_fragment
from eco230_canvas.resources import CanvasFileClient
from eco230_canvas.target import CanvasReadOnlyClient

ROOT = Path(__file__).resolve().parents[2]
BASE = 'https://uwlac.instructure.com'
COURSES = {4: 870634, 11: 869206, 12: 870121}
TITLE = 'Lab 3 Hints'
FILE = 'Lab3_Completed.twbx'
OLD_FILE = 'Lab3_Completed_EXT.twbx'
SOURCE = ROOT / 'canvas/content/files' / FILE
SITE_URL = 'https://eco230.github.io/boland_course/week03/labs/lab-3-hints.html'

def digest(data):
    return hashlib.sha256(data).hexdigest()

def validate_body(body):
    for expected in ('AVG(injuries_total)', 'At least 30', 'Table (Down)', 'weather_station_name', 'temperature_f (bin)', 'trafficway_type'):
        if expected not in body:
            raise ValueError('Missing hint: ' + expected)
    if body.count('<table') != 7 or '<script' in body or '::: {' in body:
        raise ValueError('Invalid native hints rendering')
    if body.count('Hide is different from Exclude') != 1:
        raise ValueError('Callout title missing or duplicated')

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--execute', action='store_true')
    parser.add_argument('--resume', action='store_true', help='Resume the saved preflight after an interrupted apply')
    args = parser.parse_args()
    html_path = ROOT / '_site/week03/labs/lab-3-hints.html'
    if html_path.stat().st_mtime < (ROOT / 'week03/labs/lab-3-hints.qmd').stat().st_mtime:
        raise ValueError('Render hints before synchronization')
    body = _canvas_quarto_fragment(html_path.read_text(encoding='utf-8'), SITE_URL)
    validate_body(body)
    with ZipFile(SOURCE) as archive:
        if archive.testzip() or not any(n.endswith('.twb') for n in archive.namelist()):
            raise ValueError('Invalid packaged Tableau workbook')
    sha = digest(SOURCE.read_bytes())
    reader = CanvasReadOnlyClient(BASE, os.environ['CANVAS_TOKEN'])
    writer = CanvasFileClient(BASE, os.environ['CANVAS_TOKEN'])
    work = ROOT / 'canvas/work/lab3-hints'
    work.mkdir(parents=True, exist_ok=True)
    (work / 'native-preview.html').write_text(body, encoding='utf-8')
    snapshots = json.loads((work / 'preflight.json').read_text()) if args.resume else []
    for section, cid in ([] if args.resume else COURSES.items()):
        prefix = f'/api/v1/courses/{cid}'
        course = reader.get(prefix)
        if course['course_code'] != f'ECO 230-{section:02d}' or course['workflow_state'] not in ('available', 'unpublished'):
            raise ValueError('Unexpected course')
        modules = reader.get_paginated(prefix + '/modules')
        matches = [m for m in modules if m['name'] == 'Week 3: Data Visualization']
        if len(matches) != 1 or len(modules) != 16:
            raise ValueError('Unexpected module structure')
        item_path = prefix + f"/modules/{matches[0]['id']}/items"
        items = reader.get_paginated(item_path)
        hints = [i for i in items if i['title'] == TITLE]
        solutions = [i for i in items if i['title'] in (FILE, OLD_FILE)]
        if len(hints) != 1 or len(solutions) != 1:
            raise ValueError('Expected exactly one hint and one completed workbook item')
        if hints[0]['type'] not in ('ExternalUrl', 'Page') or solutions[0]['type'] != 'File':
            raise ValueError('Unexpected item types')
        if any(i.get('completion_requirement') for i in hints + solutions):
            raise ValueError('Review completion requirements before replacing an item')
        pages = [p for p in reader.get_paginated(prefix + '/pages') if p['title'] == TITLE]
        if len(pages) > 1:
            raise ValueError('Duplicate hint pages')
        page = reader.get(prefix + '/pages/' + pages[0]['url']) if pages else None
        if page and page.get('publish_at'):
            raise ValueError('Hints page has a publication schedule')
        files = [f for f in reader.get_paginated(prefix + '/files') if f['display_name'] == FILE]
        if len(files) > 1:
            raise ValueError('Ambiguous completed workbook')
        file = files[0] if files else None
        if file:
            with urlopen(file['url']) as response:
                if digest(response.read()) != sha:
                    raise ValueError('Existing completed workbook differs')
        snapshots.append(dict(section=section, prefix=prefix, course=course, modules=modules,
            item_path=item_path, items=items, hint=hints[0], solution=solutions[0], page=page, file=file))
        print(json.dumps({'section':section,'native_page':'update' if page else 'create',
            'upload':not bool(file),'hints_published':hints[0]['published'],
            'solution_published':solutions[0]['published']}), flush=True)
    if not args.resume:
        (work / 'preflight.json').write_text(json.dumps(snapshots, indent=2), encoding='utf-8')
    if not args.execute:
        return
    def mutate(path, method, fields=None):
        return writer._canvas_request(BASE + path, method=method,
            data=urlencode(fields).encode() if fields else None,
            content_type='application/x-www-form-urlencoded')
    def replace_item(s, old, fields):
        # Create and verify replacement before removing only the old module reference.
        fields.update({'module_item[position]':old['position'], 'module_item[indent]':old.get('indent',0)})
        current = reader.get_paginated(s['item_path'])
        matches = [i for i in current if i['title'] == fields['module_item[title]'] and i['type'] == fields['module_item[type]']]
        if len(matches) > 1:
            raise ValueError('Ambiguous replacement during resume')
        new = matches[0] if matches else mutate(s['item_path'], 'POST', fields)
        # File item publication follows the file lock; Canvas rejects a PUT
        # to module_item[published] for these unpublishable=false File items.
        if new['type'] != 'File' and new['published'] != old['published']:
            new = mutate(s['item_path'] + '/' + str(new['id']), 'PUT',
                         {'module_item[published]':str(old['published']).lower()})
        if new['published'] != old['published']:
            raise ValueError('Replacement publication mismatch')
        if any(i['id'] == old['id'] for i in current) and old['id'] != new['id']:
            mutate(s['item_path'] + '/' + str(old['id']), 'DELETE')
        return new
    receipts = []
    for s in snapshots:
        cid = s['course']['id']
        if args.resume:
            current_files = [f for f in reader.get_paginated(s['prefix'] + '/files') if f['display_name'] == FILE]
            if len(current_files) > 1:
                raise ValueError('Ambiguous file during resume')
            s['file'] = current_files[0] if current_files else None
            if s['file']:
                with urlopen(s['file']['url']) as response:
                    if digest(response.read()) != sha:
                        raise ValueError('Resume file differs')
            current_pages = [p for p in reader.get_paginated(s['prefix'] + '/pages') if p['title'] == TITLE]
            if len(current_pages) > 1:
                raise ValueError('Ambiguous page during resume')
            s['page'] = current_pages[0] if current_pages else None
        file = s['file'] or writer.upload_file(str(cid), file_path=SOURCE, display_name=FILE,
            content_type='application/octet-stream', folder_path='eco230-managed/lab3')
        if file['display_name'] != FILE:
            raise ValueError('Unexpected upload rename')
        # Preserve release state of the existing solution module item.
        file = mutate(f"/api/v1/files/{file['id']}", 'PUT',
                      {'locked':str(not s['solution']['published']).lower(),'hidden':'false'})
        page = s['page']
        fields = {'wiki_page[body]':body, 'wiki_page[notify_of_update]':'false'}
        if page:
            page = mutate(s['prefix'] + '/pages/' + page['url'], 'PUT', fields)
        else:
            fields.update({'wiki_page[title]':TITLE, 'wiki_page[published]':str(s['hint']['published']).lower()})
            page = mutate(s['prefix'] + '/pages', 'POST', fields)
        if s['hint']['type'] == 'ExternalUrl':
            replace_item(s, s['hint'], {'module_item[type]':'Page', 'module_item[page_url]':page['url'], 'module_item[title]':TITLE})
        if s['solution'].get('content_id') != file['id']:
            replace_item(s, s['solution'], {'module_item[type]':'File', 'module_item[content_id]':file['id'], 'module_item[title]':FILE})
        after = reader.get(s['prefix'] + '/pages/' + page['url'])
        validate_body(after['body'])
        expected_publication = s['page']['published'] if s['page'] else s['hint']['published']
        if after['published'] != expected_publication:
            raise ValueError('Page publication changed')
        fresh_items = reader.get_paginated(s['item_path'])
        replaced = {s['hint']['id'],s['solution']['id']}
        unaffected = lambda rows: [(i['id'], i['published']) for i in rows if i['id'] not in replaced and i['title'] not in (TITLE,FILE)]
        if unaffected(fresh_items) != unaffected(s['items']) or len(fresh_items) != len(s['items']):
            raise ValueError('Unrelated module items changed')
        for old,title,kind in [(s['hint'],TITLE,'Page'),(s['solution'],FILE,'File')]:
            found = [i for i in fresh_items if i['title']==title]
            if len(found)!=1 or found[0]['type']!=kind or found[0]['position']!=old['position'] or found[0]['published']!=old['published']:
                raise ValueError('Replacement item verification failed')
        states = lambda rows: [(m['id'],m['published']) for m in rows]
        if states(reader.get_paginated(s['prefix']+'/modules')) != states(s['modules']):
            raise ValueError('Module publication changed')
        if reader.get(s['prefix'])['workflow_state'] != s['course']['workflow_state']:
            raise ValueError('Course publication changed')
        fresh_file = reader.get(f"/api/v1/files/{file['id']}")
        if fresh_file['locked'] != (not s['solution']['published']):
            raise ValueError('Solution release state mismatch')
        with urlopen(fresh_file['url']) as response:
            if digest(response.read()) != sha:
                raise ValueError('Uploaded workbook hash mismatch')
        (work / f"section-{s['section']}-verified.html").write_text(after['body'],encoding='utf-8')
        receipt = dict(section=s['section'],page_url=f"{BASE}/courses/{cid}/pages/{page['url']}",
            file_id=file['id'],sha256=sha,published=after['published'],publication_preserved=True)
        receipts.append(receipt)
        (work/'verification.json').write_text(json.dumps(receipts,indent=2),encoding='utf-8')
        print(json.dumps(receipt),flush=True)

if __name__ == '__main__':
    main()
