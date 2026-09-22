"""Update Homework 3 native instructions/rubric and order six Week 3 lab items.

Dry-run by default. Preserve scores, due dates, group membership, publication,
module membership, and all unrelated item ordering. Never writes submissions.
"""
import argparse,copy,json,os,re
from pathlib import Path
from urllib.parse import urlencode
import yaml
from eco230_canvas.content import _canvas_quarto_fragment,_rubric_fields
from eco230_canvas.resources import CanvasFileClient
from eco230_canvas.target import CanvasReadOnlyClient
ROOT=Path(__file__).resolve().parents[2]
BASE='https://uwlac.instructure.com'
COURSES={4:870634,11:869206,12:870121}
ORDER=['labprep-for-lab-3','lab-3-creating-basic-visualizations-in-excel-and-tableau','lab-3-hints','lab-3-video-guide-tableau-and-excel','Lab3_Completed.twbx','video-tutorial-opening-a-tableau-file']
SITE='https://eco230.github.io/boland_course/week03/Homework_03.html'

def item_key(item): return item.get('page_url') or item['title']
def planned_order(items):
    targets={item_key(i):i for i in items if item_key(i) in ORDER}
    if len(targets)!=6: raise ValueError('Expected all six unique lab items')
    slots=[j for j,i in enumerate(items) if item_key(i) in ORDER]
    if slots!=list(range(min(slots),max(slots)+1)): raise ValueError('Unexpected interleaved lab items')
    result=list(items)
    result[min(slots):max(slots)+1]=[targets[k] for k in ORDER]
    return result,min(i['position'] for i in targets.values())

def validate_body(body):
    if any(x in body for x in ('TipImportant',':::','quarto-screen-reader-only','<script')): raise ValueError('Unsafe or broken callout rendering')
    for x in ('4-6 sentences','all three visuals embedded','Posit Cloud is optional','Responsible Use of AI','What to turn in'):
        if x not in body: raise ValueError('Missing assignment requirement: '+x)
    if body.count('border-left:')!=2: raise ValueError('Expected two native callouts')

def rubric_fields(source,existing,aid):
    identity_fields=[]
    for criterion in existing:
        prefix=f"rubric[criteria][{criterion['id']}]"
        identity_fields.append((prefix+'[id]',str(criterion['id'])))
        for rating in criterion['ratings']:
            identity_fields.append((prefix+f"[ratings][{rating['id']}][id]",str(rating['id'])))
    if len(existing)==len(source['criteria']):
        return _rubric_fields(source,association_id=aid,association_type='Assignment',existing_criteria=existing)+identity_fields
    if len(existing)!=6 or len(source['criteria'])!=7: raise ValueError('Unexpected rubric structure')
    old=copy.deepcopy(source);old['criteria']=old['criteria'][:-1]
    fields=_rubric_fields(old,association_id=aid,association_type='Assignment',existing_criteria=existing)
    new=copy.deepcopy(source);new['criteria']=new['criteria'][-1:]
    for key,value in _rubric_fields(new,association_id=aid,association_type='Assignment'):
        if key.startswith('rubric[criteria][0]'):
            fields.append((key.replace('rubric[criteria][0]','rubric[criteria][new_ai_reflection]',1),value))
    return fields+identity_fields

def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--execute',action='store_true');args=p.parse_args()
    html=ROOT/'_site/week03/Homework_03.html'
    if html.stat().st_mtime<(ROOT/'week03/Homework_03.qmd').stat().st_mtime: raise ValueError('Render current QMD first')
    body=_canvas_quarto_fragment(html.read_text(encoding='utf-8'),SITE);validate_body(body)
    rubric=yaml.safe_load((ROOT/'canvas/manifests/rubrics/homework-3.yml').read_text(encoding='utf-8'))
    if sum(c['points'] for c in rubric['criteria'])!=100: raise ValueError('Rubric total must remain 100')
    r=CanvasReadOnlyClient(BASE,os.environ['CANVAS_TOKEN']);w=CanvasFileClient(BASE,os.environ['CANVAS_TOKEN'])
    work=ROOT/'canvas/work/homework3-refresh';work.mkdir(exist_ok=True)
    snapshots=[]
    for section,cid in COURSES.items():
        prefix=f'/api/v1/courses/{cid}';course=r.get(prefix)
        if course['course_code']!=f'ECO 230-{section:02d}' or course['workflow_state'] not in ('available','unpublished'): raise ValueError('Wrong course')
        modules=r.get_paginated(prefix+'/modules');matches=[m for m in modules if m['name']=='Week 3: Data Visualization']
        if len(matches)!=1 or len(modules)!=16: raise ValueError('Unexpected modules')
        ip=prefix+f"/modules/{matches[0]['id']}/items";items=r.get_paginated(ip);desired,start=planned_order(items)
        assignments=[a for a in r.get_paginated(prefix+'/assignments') if a['name'].startswith('Homework 3:')]
        if len(assignments)!=1: raise ValueError('Ambiguous Homework 3')
        a=r.get(prefix+'/assignments/'+str(assignments[0]['id']))
        if a['published'] or a['points_possible']!=100: raise ValueError('Expected unpublished 100-point Homework 3')
        rb=r.get(prefix+'/rubrics/'+str(a['rubric_settings']['id'])+'?include[]=associations')
        if rb.get('read_only') or len(rb['associations'])!=1 or rb['associations'][0]['association_id']!=a['id']: raise ValueError('Shared or read-only rubric')
        # Recover the original six IDs if a prior interrupted attempt rebuilt
        # them; retain the newly added reflection criterion on resumed runs.
        original_path=work/f'section-{section}-before.json'
        original=json.loads(original_path.read_text(encoding='utf-8'))['rubric'] if original_path.exists() else rb
        if original['id']!=rb['id']: raise ValueError('Rubric identity differs from baseline')
        existing=copy.deepcopy(rb['data'])
        if len(original['data'])==6:
            for i in range(6):
                existing[i]['id']=original['data'][i]['id']
                for rating,old_rating in zip(existing[i]['ratings'],original['data'][i]['ratings']):
                    rating['id']=old_rating['id']
        fields=rubric_fields(rubric,existing,a['id'])
        native=body.replace('https://eco230.github.io/boland_course/week03/labs/lab-3-hints.html',f'/courses/{cid}/pages/lab-3-hints')
        (work/f'section-{section}-preview.html').write_text(native,encoding='utf-8')
        s=dict(section=section,prefix=prefix,course=course,modules=modules,items=items,desired=desired,start=start,item_path=ip,assignment=a,rubric=rb,expected_ids=[c['id'] for c in existing[:6]],fields=fields,body=native)
        snapshots.append(s)
        print(json.dumps({'section':section,'assignment_id':a['id'],'lab_order':ORDER,'rubric_points':100,'ai_points':5,'published':a['published']}),flush=True)
    (work/'reviewed-plan.json').write_text(json.dumps(snapshots,indent=2),encoding='utf-8')
    if not args.execute:return
    def mutate(path,fields):return w._canvas_request(BASE+path,method='PUT',data=urlencode(fields).encode(),content_type='application/x-www-form-urlencoded')
    receipts=[]
    for s in snapshots:
        a=s['assignment'];prefix=s['prefix'];ap=prefix+'/assignments/'+str(a['id'])
        current=r.get(ap)
        if current['description']!=a['description'] or current['published']!=a['published']: raise ValueError('Assignment changed since planning')
        mutate(ap,{'assignment[description]':s['body']})
        mutate(prefix+'/rubrics/'+str(s['rubric']['id']),s['fields'])
        targets={item_key(i):i for i in s['items'] if item_key(i) in ORDER}
        for offset,key in enumerate(ORDER):
            mutate(s['item_path']+'/'+str(targets[key]['id']),{'module_item[position]':s['start']+offset})
        after=r.get(ap);validate_body(after['description'])
        for key in ('points_possible','published','assignment_group_id','due_at','unlock_at','lock_at','submission_types'):
            if after.get(key)!=a.get(key): raise ValueError('Assignment setting changed: '+key)
        rb=r.get(prefix+'/rubrics/'+str(after['rubric_settings']['id'])+'?include[]=associations')
        if rb['id']!=s['rubric']['id'] or len(rb['data'])!=7 or rb['points_possible']!=100: raise ValueError('Rubric verification failed')
        for actual,expected in zip(rb['data'],rubric['criteria']):
            if actual['description']!=expected['description'] or actual['points']!=expected['points']: raise ValueError('Rubric criterion mismatch')
        if [c['id'] for c in rb['data'][:6]]!=s['expected_ids']: raise ValueError('Existing rubric criterion IDs changed')
        if len(rb['associations'])!=1 or not rb['associations'][0]['use_for_grading'] or rb['associations'][0]['association_id']!=a['id']: raise ValueError('Grading association changed')
        final_items=r.get_paginated(s['item_path'])
        if [i['id'] for i in final_items]!=[i['id'] for i in s['desired']]: raise ValueError('Module order mismatch')
        states=lambda rows:sorted((i['id'],i.get('published')) for i in rows)
        if states(final_items)!=states(s['items']) or states(r.get_paginated(prefix+'/modules'))!=states(s['modules']): raise ValueError('Publication changed')
        if r.get(prefix)['workflow_state']!=s['course']['workflow_state']:raise ValueError('Course state changed')
        (work/f"section-{s['section']}-verified.html").write_text(after['description'],encoding='utf-8')
        receipt=dict(section=s['section'],assignment_url=after['html_url'],rubric_id=rb['id'],ai_points=5,points=100,order_verified=True,publication_preserved=True)
        receipts.append(receipt);(work/'verification.json').write_text(json.dumps(receipts,indent=2),encoding='utf-8');print(json.dumps(receipt),flush=True)
if __name__=='__main__':main()
