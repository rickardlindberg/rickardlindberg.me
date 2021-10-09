#!/usr/bin/env python3

import sys
import subprocess

def process_line(line):
    if line.startswith("$:shell:"):
        return shell(*line.strip().split(":")[2:])
    else:
        return [line]

def shell(cwd, cmd):
    return [
        f"    {x}\n"
        for x
        in [f"$ {cmd}"]+subprocess.check_output(cmd, cwd=cwd, shell=True, text=True).splitlines(False)
    ]

for path in sys.argv[1:]:
    with open(path) as f:
        for line in f:
            for output in process_line(line):
                sys.stdout.write(output)
