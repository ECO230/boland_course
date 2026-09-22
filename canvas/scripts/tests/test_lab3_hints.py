import importlib.util
import unittest
from pathlib import Path
spec=importlib.util.spec_from_file_location('hints',Path(__file__).resolve().parents[1]/'sync-lab3-hints.py')
hints=importlib.util.module_from_spec(spec)
spec.loader.exec_module(hints)
class NativeHintsTests(unittest.TestCase):
    def setUp(self):
        self.body=hints._canvas_quarto_fragment((hints.ROOT/'_site/week03/labs/lab-3-hints.html').read_text(encoding='utf-8'),hints.SITE_URL)
    def test_native_tables_and_callouts_survive(self):
        hints.validate_body(self.body)
        self.assertIn('border-left:',self.body)
    def test_rejects_missing_demo_transformation(self):
        with self.assertRaises(ValueError):
            hints.validate_body(self.body.replace('At least 30','Any count'))
    def test_rejects_executable_script(self):
        with self.assertRaises(ValueError):
            hints.validate_body(self.body+'<script>alert(1)</script>')
if __name__=='__main__': unittest.main()
