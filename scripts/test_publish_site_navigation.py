import unittest
from publish_site_navigation import patch_navigation,target_path,selections
HEAD='<html><head><title>Keep title</title></head><body><header id="quarto-header"><ul class="dropdown-menu" aria-labelledby="nav-menu-homework"><li><a class="dropdown-item" href="../week03/Homework_03.html"><span class="dropdown-text">Homework 3</span></a></li></ul></header><main>Keep body and code</main></body></html>'
ITEM={'menu':'Homework','label':'Homework 5','target':'week06/Homework_05.html','order':['Homework 1','Homework 2','Homework 3','Homework 4','Homework 5']}
class NavigationTest(unittest.TestCase):
 def test_legacy_gap_inserts_without_changing_content(self):
  result,count=patch_navigation(HEAD,[ITEM],page_path='syllabus/page.html')
  self.assertEqual(count,1);self.assertIn('Homework 5',result);self.assertIn('<main>Keep body and code</main>',result);self.assertIn('<title>Keep title</title>',result)
  self.assertLess(result.index('Homework 3'),result.index('Homework 5'))
 def test_existing_valid_relative_link_is_unchanged(self):
  page=HEAD.replace('Homework_03.html','Homework_05.html').replace('week03','week06').replace('Homework 3','Homework 5')
  self.assertEqual(patch_navigation(page,[ITEM],page_path='syllabus/page.html'),(page,1))
 def test_wrong_link_is_fixed_without_duplicate_item(self):
  page=HEAD.replace('Homework 3','Homework 5');result,_=patch_navigation(page,[ITEM],page_path='syllabus/page.html');self.assertEqual(result.count('>Homework 5<'),1);self.assertIn('week06/Homework_05.html',result)
 def test_case_colliding_pages_keep_distinct_bodies(self):
  for body in ['Uppercase page','Lowercase page']:
   page=HEAD.replace('Keep body and code',body);result,_=patch_navigation(page,[ITEM]);self.assertIn('<main>'+body+'</main>',result)
 def test_private_or_escaping_targets_rejected(self):
  for path in ['../private/quiz.qmd','canvas/work/private.qmd','.assignment-build/hw.qmd']:
   with self.assertRaises(ValueError):target_path({'file':path})
 def test_unknown_source_entry_rejected(self):
  with self.assertRaises(ValueError):selections({'website':{'navbar':{'left':[{'text':'Slides','menu':[{'text':'Week 5','file':'week05/week_05.qmd'}]}]}}},['Slides=Week 6'])
 def test_move_to_restored_menu_preserves_other_links_and_body(self):
  page=HEAD.replace('<ul class="dropdown-menu"', '<ul class="dropdown-menu"',1).replace('<header id="quarto-header">','<header id="quarto-header"><li class="nav-item dropdown"><a id="nav-menu-slides"></a><ul aria-labelledby="nav-menu-slides"><li><a class="dropdown-item" href="../week07/interpretation_x2.html"><span class="dropdown-text">Wrong label</span></a></li></ul></li><li class="nav-item dropdown"><a id="nav-menu-homework"></a>')
  item={'menu':'Interpretation','label':'Chi-square Tests','target':'week07/interpretation_x2.html','order':['Chi-square Tests'],'menu_order':['Slides','Interpretation','Homework']}
  result,count=patch_navigation(page,[item],page_path='syllabus/page.html')
  self.assertEqual(count,1);self.assertNotIn('Wrong label',result);self.assertEqual(result.count('interpretation_x2.html'),1)
  self.assertLess(result.index('id="nav-menu-interpretation"'),result.index('id="nav-menu-homework"'))
  self.assertIn('Homework 3',result);self.assertIn('<main>Keep body and code</main>',result)
  self.assertEqual(patch_navigation(result,[item],page_path='syllabus/page.html'),(result,1))
if __name__=='__main__':unittest.main()
