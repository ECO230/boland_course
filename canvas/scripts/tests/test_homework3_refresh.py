import importlib.util,json,unittest,yaml
from pathlib import Path
spec=importlib.util.spec_from_file_location('hw3',Path(__file__).resolve().parents[1]/'refresh-homework3.py');hw3=importlib.util.module_from_spec(spec);spec.loader.exec_module(hw3)
class Homework3Tests(unittest.TestCase):
 def test_order_keeps_nonlab_items(self):
  items=[dict(id=0,title='Lab',position=7,published=True)]+[dict(id=j+1,title=k,page_url=k,position=j+8,published=False) for j,k in enumerate(reversed(hw3.ORDER))]+[dict(id=9,title='Homework',position=14,published=True)]
  result,_=hw3.planned_order(items)
  self.assertEqual([hw3.item_key(i) for i in result[1:-1]],hw3.ORDER)
  self.assertEqual([result[0],result[-1]],[items[0],items[-1]])
 def test_interleaved_items_are_rejected(self):
  items=[dict(id=j,title=k,page_url=k,position=j) for j,k in enumerate(hw3.ORDER)]
  items.insert(2,dict(id=20,title='Unexpected',position=2))
  with self.assertRaises(ValueError):hw3.planned_order(items)
 def test_rubric_preserves_existing_ids_and_adds_ai(self):
  source=yaml.safe_load((hw3.ROOT/'canvas/manifests/rubrics/homework-3.yml').read_text(encoding='utf-8'))
  existing=[dict(id=f'criterion{i}',ratings=[dict(id=f'rating{j}') for j in range(len(c['ratings']))]) for i,c in enumerate(source['criteria'][:6])]
  fields=dict(hw3.rubric_fields(source,existing,10))
  self.assertIn('rubric[criteria][criterion0][ratings][rating0][points]',fields)
  self.assertEqual(fields['rubric[criteria][criterion0][id]'],'criterion0')
  self.assertEqual(fields['rubric[criteria][criterion0][ratings][rating0][id]'],'rating0')
  self.assertEqual(fields['rubric[criteria][new_ai_reflection][points]'],'5.0')
  self.assertEqual(sum(c['points'] for c in source['criteria']),100)
 def test_native_body(self):
  body=hw3._canvas_quarto_fragment((hw3.ROOT/'_site/week03/Homework_03.html').read_text(encoding='utf-8'),hw3.SITE)
  hw3.validate_body(body)
  with self.assertRaises(ValueError):hw3.validate_body(body+'TipImportant')
if __name__=='__main__':unittest.main()
