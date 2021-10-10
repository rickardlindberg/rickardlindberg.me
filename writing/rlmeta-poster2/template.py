#!/usr/bin/env python3

import sys
import subprocess

def process_line(line):
    if line.startswith("$:shell:"):
        return shell(*line.strip().split(":")[2:])
    elif line.startswith("$:code:"):
        return code(*line.strip().split(":")[2:])
    else:
        return [line]

def shell(cwd, cmd):
    return [
        f"    {x}\n"
        for x
        in [f"$ {cmd}"]+subprocess.check_output(cmd, cwd=cwd, shell=True, text=True).splitlines(False)
    ]

def code(path):
    pygments_cmd = ["pygmentize"]
    if path.endswith(".rlmeta"):
        pygments_cmd.extend(["-l", "rlmeta_lexer.py:RLMetaLexer", "-x"])
    pygments_cmd.extend(["-f", "html"])
    pygments_cmd.append(path)
    return subprocess.check_output(pygments_cmd, text=True).splitlines(True)

for path in sys.argv[1:]:
    with open(path) as f:
        for line in f:
            for output in process_line(line):
                sys.stdout.write(output)
