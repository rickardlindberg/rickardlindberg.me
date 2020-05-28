#!/usr/bin/env python

import os
import subprocess
import sys

def make_next_version():
    intermediate_compilers = meta_compile_rlmeta()
    final_compiler = intermediate_compilers.pop(-1)
    test(final_compiler)
    mv(final_compiler, "rlmeta.py")
    for compiler in intermediate_compilers:
        rm(compiler)
    log("OK!")

def test(rlmeta):
    log("Test: Has its own support library")
    assert run_rlmeta(rlmeta, ["--support"]) == read("support.py")
    log("Test: Disallow semantic action in the middle")
    run_rlmeta(rlmeta, [], "Grammar { x = . -> [] . }", expect_failure=True)

def meta_compile_rlmeta():
    compiler = "rlmeta.py"
    content = read(compiler)
    intermediate_compilers = []
    for i in range(4):
        next_compiler = "rlmeta{}.py".format(i+1)
        log("Compiling {} -> {}".format(compiler, next_compiler))
        next_content = compile_rlmeta(compiler)
        write(next_compiler, next_content)
        intermediate_compilers.append(next_compiler)
        if next_content == content:
            return intermediate_compilers
        compiler = next_compiler
        content = next_content
    fail("Unable to produce metacompiler.")

def compile_rlmeta(rlmeta):
    return run_rlmeta(rlmeta, [
        "--embed", "SUPPORT", "support.py",
        "--support",
        "--compile", "parser.rlmeta",
        "--compile", "codegenerator.rlmeta",
        "--copy", "main.py",
    ])

def run_rlmeta(rlmeta, args, stdin="", expect_failure=False):
    process = subprocess.Popen(
        ["python", rlmeta]+args,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE
    )
    stdout, _ = process.communicate(stdin)
    if expect_failure:
        if process.returncode == 0:
            fail("Expected failure")
    else:
        if process.returncode != 0:
            fail("Expected success")
    return stdout

def mv(src, dest):
    log("Move {} -> {}".format(src, dest))
    os.remove(dest)
    os.rename(src, dest)

def rm(path):
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
    if sys.argv[1:] == ["compile"]:
        sys.stdout.write(compile_rlmeta("rlmeta.py"))
    else:
        make_next_version()
