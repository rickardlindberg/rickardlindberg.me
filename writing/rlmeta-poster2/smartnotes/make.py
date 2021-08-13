#!/usr/bin/env python

import os
import subprocess
import sys

RLMETA_COMBINED_SUPPORT = "rlmeta_combined_support.py"

def make_next_version():
    final_compiler = meta_compile_rlmeta()
    test(final_compiler)
    mv(final_compiler, "rlmeta.py")

def meta_compile_rlmeta():
    compiler = "rlmeta.py"
    content = read(compiler)
    for i in range(4):
        next_compiler = "rlmeta{}.py".format(i+1)
        next_content = compile_rlmeta(compiler)
        log("Writing {}".format(next_compiler))
        write(next_compiler, next_content)
        if next_content == content:
            return next_compiler
        compiler = next_compiler
        content = next_content
    fail("Unable to produce metacompiler.")

def compile_rlmeta(rlmeta):
    log("Compiling pyvm")
    write("pyvm.py", run_rlmeta(rlmeta, [
        "--support",
        "--compile", "pyvm/parser.rlmeta",
        "--compile", "pyvm/codegenerator.rlmeta",
        "--copy", "pyvm/main.py",
    ]))
    log("Compiling vm")
    vm = run_rlmeta("pyvm.py", ["rlmeta/vm.pyvm"])
    log("Writing combined support library")
    write(RLMETA_COMBINED_SUPPORT, vm+read("rlmeta/support.py"))
    log("Compiling rlmeta using {}".format(rlmeta))
    return run_rlmeta(rlmeta, [
        "--embed", "SUPPORT", RLMETA_COMBINED_SUPPORT,
        "--support",
        "--compile", "rlmeta/parser.rlmeta",
        "--compile", "rlmeta/codegenerator.rlmeta",
        "--copy", "rlmeta/main.py",
    ])

def test(rlmeta):
    log("Test: Has its own support library")
    assert run_rlmeta(rlmeta, ["--support"]) == read(RLMETA_COMBINED_SUPPORT)
    log("Test: Disallow semantic action in the middle")
    run_rlmeta(rlmeta, [], b"Grammar { x = . -> [] . }", expect_failure=True)

def run_rlmeta(rlmeta, args, stdin=b"", expect_failure=False):
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
    log("Moving {} -> {}".format(src, dest))
    os.remove(dest)
    os.rename(src, dest)

def cleanup():
    for path in [
        "rlmeta1.py",
        "rlmeta2.py",
        "rlmeta3.py",
        "rlmeta4.py",
        "pyvm.py",
        RLMETA_COMBINED_SUPPORT,
    ]:
        if os.path.exists(path):
            log("Deleting {}".format(path))
            os.remove(path)

def read(path):
    with open(path, "rb") as f:
        return f.read()

def write(path, content):
    with open(path, "wb") as f:
        return f.write(content)

def log(message):
    sys.stdout.write("\033[0;33m{}\033[0m\n".format(message))

def success(message):
    sys.stdout.write("\033[0;32m{}\033[0m\n".format(message))

def fail(message):
    sys.exit("\033[0;31mERROR: {}\033[0m".format(message))

if __name__ == "__main__":
    cleanup()
    if sys.argv[1:] == ["compile"]:
        sys.stdout.buffer.write(compile_rlmeta("rlmeta.py"))
    else:
        make_next_version()
    cleanup()
    success("  O-----------------O")
    success("  | RLMeta compiled |")
    success("~~|     itself!     |")
    success("  O-----------------O")
