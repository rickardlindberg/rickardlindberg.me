#!/usr/bin/env python

import os
import subprocess
import sys

def make_next_version():
    final_compiler = meta_compile_rlmeta()
    test(final_compiler)
    mv(final_compiler, "rlmeta.py")
    log("OK!")

def meta_compile_rlmeta():
    compiler = "rlmeta.py"
    content = read(compiler)
    for i in range(4):
        next_compiler = "rlmeta{}.py".format(i+1)
        log("Compiling {} -> {}".format(compiler, next_compiler))
        next_content = compile_rlmeta(compiler)
        write(next_compiler, next_content)
        if next_content == content:
            return next_compiler
        compiler = next_compiler
        content = next_content
    fail("Unable to produce metacompiler.")

def compile_rlmeta(rlmeta):
    return run_rlmeta(rlmeta, [
        "--embed", "SUPPORT", "rlmeta/support.py",
        "--support",
        "--compile", "rlmeta/parser.rlmeta",
        "--compile", "rlmeta/codegenerator.rlmeta",
        "--copy", "rlmeta/main.py",
    ])

def test(rlmeta):
    log("Test: Has its own support library")
    assert run_rlmeta(rlmeta, ["--support"]) == read("rlmeta/support.py")
    log("Test: Disallow semantic action in the middle")
    run_rlmeta(rlmeta, [], "Grammar { x = . -> [] . }", expect_failure=True)

def run_rlmeta(rlmeta, args, stdin="", expect_failure=False):
    process = subprocess.Popen(
        ["python", rlmeta]+args,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE
    )
    stdout, _ = process.communicate(stdin.encode("utf-8"))
    if expect_failure:
        if process.returncode == 0:
            fail("Expected failure")
    else:
        if process.returncode != 0:
            fail("Expected success")
    return stdout.decode("utf-8")

def mv(src, dest):
    log("Move {} -> {}".format(src, dest))
    os.remove(dest)
    os.rename(src, dest)

def cleanup():
    for path in ["rlmeta1.py", "rlmeta2.py", "rlmeta3.py", "rlmeta4.py"]:
        if os.path.exists(path):
            log("Delete {}".format(path))
            os.remove(path)

def read(path):
    with open(path) as f:
        return f.read()

def write(path, content):
    with open(path, "w") as f:
        return f.write(content)

def log(message):
    sys.stdout.write("\033[0;33m{}\033[0m\n".format(message))

def fail(message):
    sys.exit("\033[0;31mERROR: {}\033[0m".format(message))

if __name__ == "__main__":
    cleanup()
    if sys.argv[1:] == ["compile"]:
        sys.stdout.write(compile_rlmeta("rlmeta.py"))
    else:
        make_next_version()
    cleanup()
