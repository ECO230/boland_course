"""Plan, publish, and verify selected Quarto navbar entries without rerendering pages."""
from __future__ import annotations
import argparse,hashlib,html,json,re,subprocess,urllib.request
from datetime import datetime
from pathlib import Path,PurePosixPath
from urllib.parse import urljoin,urlsplit
import yaml
ROOT=Path(__file__).resolve().parents[1]
SITE='https://eco230.github.io/boland_course/'
HEADER=re.compile(r'<header\b[^>]*\bid=["\']quarto-header["\'][^>]*>.*?</header>',re.S)
ITEM=re.compile(r'<li\b[^>]*>\s*<a\b[^>]*class=["\'][^"\']*\bdropdown-item\b[^"\']*["\'][^>]*>.*?</a>\s*</li>',re.S)
LABEL=re.compile(r'<span\b[^>]*class=["\'][^"\']*\bdropdown-text\b[^"\']*["\'][^>]*>(.*?)</span>',re.S)
def require(ok,message):
 if not ok:raise ValueError(message)
def git(directory,*args,input=None):
 command=['git','-c','safe.directory='+str(ROOT),'-c','safe.directory='+str(directory),'-C',str(directory),*args]
 return subprocess.run(command,input=input,stdout=subprocess.PIPE,stderr=subprocess.PIPE,check=True).stdout
def digest(path):return hashlib.sha256(path.read_bytes()).hexdigest()
def save(path,value):path.write_text(json.dumps(value,indent=2)+'\n',encoding='utf8')
def target_path(item):
 value=item.get('file') or item.get('href');require(isinstance(value,str),'Menu entry needs file or href')
 parsed=urlsplit(value);require(not parsed.scheme and not parsed.netloc and not parsed.query and not parsed.fragment,'This command handles local page links only')
 path=PurePosixPath(value);require(not path.is_absolute() and '..' not in path.parts,'Page path escapes the site')
 require(path.parts[0] not in {'.assignment-build','canvas','posit_assignments','group_projects'},'Nonpublic target')
 return str(path.with_suffix('.html')) if path.suffix=='.qmd' else str(path)
def selections(config,values):
 result=[]
 for value in values:
  menu,label=value.split('=',1);matches=[m for m in config['website']['navbar']['left'] if m.get('text')==menu]
  require(len(matches)==1 and 'menu' in matches[0],'Unknown navbar menu: '+menu)
  entries=matches[0]['menu'];selected=[e for e in entries if e.get('text')==label];require(len(selected)==1,'Entry missing from _quarto.yml: '+value)
  result.append({'menu':menu,'label':label,'target':target_path(selected[0]),'order':[e['text'] for e in entries if 'text' in e]})
 require(len({(s['menu'],s['label']) for s in result})==len(result),'Duplicate requested entry')
 return result
def label_of(item):
 match=LABEL.search(item)
 return html.unescape(re.sub('<[^>]*>','',match.group(1))).strip() if match else None
def patch_navigation(document,selected,site=SITE,page_path="index.html"):
 header=HEADER.search(document)
 if not header:return document,0
 content=header.group();checked=0
 for selected_item in selected:
  menu_id='nav-menu-'+re.sub(r'[^a-z0-9]+','-',selected_item['menu'].lower()).strip('-')
  pattern=re.compile(r'(<ul\b[^>]*aria-labelledby=["\']'+re.escape(menu_id)+r'["\'][^>]*>)(.*?)(</ul>)',re.S)
  match=pattern.search(content)
  if not match:continue
  inner=match.group(2);require('<ul' not in inner,'Nested menus need a rendered navigation refresh')
  items=list(ITEM.finditer(inner));labels=[label_of(i.group()) for i in items];wanted=selected_item['label'];url=urljoin(site,selected_item['target'])
  require(labels.count(wanted)<=1,'Duplicate existing label: '+wanted)
  if wanted in labels:
   item=items[labels.index(wanted)];existing_href=re.search(r'\bhref=(["\'])(.*?)\1',item.group());require(existing_href,'Menu item lacks href')
   if urljoin(urljoin(site,page_path),html.unescape(existing_href.group(2)))==url:
    checked+=1;continue
   new_item=re.sub(r'\bhref=(["\'])(.*?)\1',lambda _: 'href="'+html.escape(url,quote=True)+'"',item.group(),count=1)
   inner=inner[:item.start()]+new_item+inner[item.end():]
  else:
   new_item='<li><a class="dropdown-item" href="'+html.escape(url,quote=True)+'"><span class="dropdown-text">'+html.escape(wanted)+'</span></a></li>'
   order=selected_item['order'];index=order.index(wanted);previous=[x for x in order[:index] if x in labels];following=[x for x in order[index+1:] if x in labels]
   position=items[labels.index(previous[-1])].end() if previous else (items[labels.index(following[0])].start() if following else len(inner))
   inner=inner[:position]+'\n'+new_item+'\n'+inner[position:]
  content=content[:match.start()]+match.group(1)+inner+match.group(3)+content[match.end():];checked+=1
 result=document[:header.start()]+content+document[header.end():]
 require(HEADER.sub('',result,count=1)==HEADER.sub('',document,count=1),'Changed content outside navbar header')
 return result,checked
def origin_sha():return git(ROOT,'ls-remote','origin','refs/heads/gh-pages').decode().split()[0]
def main():
 ap=argparse.ArgumentParser(description=__doc__);ap.add_argument('--run-root',required=True,type=Path);ap.add_argument('--link',action='append',default=[],help='Exact _quarto.yml menu and label, e.g. Slides=Week 6');mode=ap.add_mutually_exclusive_group();mode.add_argument('--execute',action='store_true');mode.add_argument('--verify',action='store_true');args=ap.parse_args()
 run=(ROOT/args.run_root).resolve();require((ROOT/'canvas/work').resolve() in run.parents,'Run directory must be below canvas/work');run.mkdir(parents=True,exist_ok=True);planpath=run/'plan.json';config_path=ROOT/'_quarto.yml'
 if not args.execute and not args.verify:
  require(args.link,'Provide at least one --link');require(not planpath.exists(),'Plan already exists; reuse it or choose a new run directory')
  selected=selections(yaml.safe_load(config_path.read_text(encoding='utf8')),args.link);git(ROOT,'fetch','origin','gh-pages');base=git(ROOT,'rev-parse','origin/gh-pages').decode().strip();tree=run/'checkout';branch='codex/site-nav-'+datetime.now().strftime('%Y%m%d-%H%M%S')+'-'+hashlib.sha256(str(run).encode()).hexdigest()[:6]
  git(ROOT,'worktree','add','-b',branch,str(tree),'origin/gh-pages');paths=git(tree,'ls-files','*.html').decode().splitlines()
  for item in selected:require(item['target'] in paths,'Target HTML is not published; render and deploy it first: '+item['target'])
  changes=[];checked=[];menu_counts={}
  # Read Git objects rather than filesystem copies: Windows case-colliding
  # legacy filenames must keep their distinct page bodies.
  for path in paths:
   body=git(tree,'show','HEAD:'+path).decode('utf8');updated,count=patch_navigation(body,selected,SITE,path)
   if count:checked.append(path);menu_counts[path]=count
   if updated!=body:
    blob=git(tree,'hash-object','-w','--stdin',input=updated.encode()).decode().strip();changes.append({'path':path,'blob':blob})
  require('index.html' in checked,'Home page navbar not found')
  plan={'config_sha256':digest(config_path),'base':base,'worktree':str(tree),'branch':branch,'site':SITE,'selected':selected,'checked_pages':checked,'menu_counts':menu_counts,'changes':changes};save(planpath,plan);print(json.dumps({'status':'planned','checked_pages':len(checked),'changed_pages':len(changes),'plan':str(planpath)}));return
 plan=json.loads(planpath.read_text(encoding='utf8'));require(digest(config_path)==plan['config_sha256'],'Navigation source changed; make a new plan');tree=Path(plan['worktree'])
 if args.execute:
  require(git(tree,'rev-parse','HEAD').decode().strip()==plan['base'],'Release HEAD changed')
  require(origin_sha()==plan['base'],'Remote deployment advanced; make a fresh plan')
  require(not git(tree,'diff','--cached','--name-only').strip(),'Release index contains unrelated staged changes')
  for item in plan['changes']:git(tree,'update-index','--cacheinfo','100644,'+item['blob']+','+item['path'])
  require(set(git(tree,'diff','--cached','--name-only').decode().splitlines())=={x['path'] for x in plan['changes']},'Staged paths differ from the approved list')
  if plan['changes']:
   git(tree,'commit','-m','Publish requested website navigation entries');plan['local_commit']=git(tree,'rev-parse','HEAD').decode().strip();save(planpath,plan);git(tree,'push','origin','HEAD:gh-pages')
  plan['published_commit']=git(tree,'rev-parse','HEAD').decode().strip();save(planpath,plan);print(json.dumps({'status':'pushed' if plan['changes'] else 'unchanged','commit':plan['published_commit'],'changed_pages':len(plan['changes']),'next':'Run the same command with --verify after GitHub Pages builds.'}));return
 expected=plan.get('published_commit') or plan['base'];build=json.loads(subprocess.check_output(['gh','api','repos/ECO230/boland_course/pages/builds/latest']))
 require(build.get('commit')==expected and build.get('status')=='built','Expected Pages build is not complete; wait briefly and retry --verify')
 pages=list(dict.fromkeys(['index.html']+[item['path'] for item in plan['changes']]+[p for p in plan['checked_pages'] if p!='index.html'][:2]))
 for path in pages:
  with urllib.request.urlopen(urljoin(plan['site'],path),timeout=20) as response:body=response.read().decode('utf8')
  updated,count=patch_navigation(body,plan['selected'],plan['site'],path)
  expected_count=plan.get('menu_counts',{}).get(path)
  if expected_count is None:
   original=git(tree,'show',plan['base']+':'+path).decode('utf8');_,expected_count=patch_navigation(original,plan['selected'],plan['site'],path)
  require(count==expected_count and updated==body,'Live menu differs: '+path)
 for selected_item in plan['selected']:
  with urllib.request.urlopen(urljoin(plan['site'],selected_item['target']),timeout=20) as response:require(response.status==200,'Target page did not load')
 receipt={'status':'verified','commit':expected,'checked_pages':len(plan['checked_pages']),'changed_pages':len(plan['changes']),'live_pages':pages,'targets':[s['target'] for s in plan['selected']]};save(run/'receipt.json',receipt);print(json.dumps(receipt))
if __name__=='__main__':main()
