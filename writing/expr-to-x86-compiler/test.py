import subprocess
import sys

def eval_py(expr):
    return eval(expr)

def eval_gas(expr):
    return _eval_script(expr, "gas.sh")

def eval_native(expr):
    return _eval_script(expr, "native.sh")

def _eval_script(expr, script):
    p = subprocess.Popen(
        ["bash", script],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE
    )
    (stdout, stderr) = p.communicate(expr)
    return int(stdout.splitlines()[-1].strip())

def run_test(expr, expected_result):
    print("{} = {}".format(expr, expected_result))
    for name, fn in [("py", eval_py), ("gas", eval_gas), ("native", eval_native)]:
        actual = fn(expr)
        if actual == expected_result:
            print("  {} ok".format(name.ljust(6)))
        else:
            print("  {} fail (actual {})".format(name.ljust(6), actual))

for test_spec in sys.stdin.read().splitlines():
    expr, expected_result = test_spec.split("=")
    run_test(expr.strip(), int(expected_result.strip()))
