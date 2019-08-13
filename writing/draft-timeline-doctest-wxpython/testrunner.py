import doctest
import sys
import unittest

def load_test_cases_from_module_name(suite, module_name):
    __import__(module_name)
    module = sys.modules[module_name]
    module_suite = unittest.defaultTestLoader.loadTestsFromModule(module)
    suite.addTest(module_suite)

def load_doc_tests_from_module_name(suite, module_name):
    __import__(module_name)
    module = sys.modules[module_name]
    try:
        module_suite = doctest.DocTestSuite(module)
    except ValueError:
        # No tests found
        pass
    else:
        suite.addTest(module_suite)

if __name__ == "__main__":
    suite = unittest.TestSuite()
    load_test_cases_from_module_name(suite, "test_wx")
    load_doc_tests_from_module_name(suite, "test_doc")
    print(unittest.TextTestRunner().run(suite))
