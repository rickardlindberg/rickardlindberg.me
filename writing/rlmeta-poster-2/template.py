#!/usr/bin/env python3

import os
import re
import subprocess
import sys
import textwrap

block = None

def process_line(line):
    global block
    if block is not None:
        if line.strip() == "$:endfile":
            with open(block["path"], "w") as f:
                f.write("".join(block["lines"]))
            block = None
        else:
            block["lines"].append(line)
        return []
    if line.startswith("$:shell:"):
        return shell(*line.strip().split(":")[2:])
    elif line.startswith("$~shell~"):
        cwd, cmd = line.strip().split("~")[2:]
        subprocess.check_output(cmd, cwd=cwd, shell=True)
        return []
    elif line.startswith("$:code:"):
        return code(*line.strip().split(":")[2:])
    elif line.startswith("$:file:"):
        block = {"path": line.strip().split(":")[2], "lines": []}
        return []
    else:
        return [line]

def shell(cwd, cmd, lexer="text"):
    return "".join([
        pygmentize("$ ", "text", strip_end=True),
        pygmentize(cmd, "bash", strip_beginning=True, strip_end=True),
        '\n',
        pygmentize(
            subprocess.check_output(cmd, stderr=subprocess.STDOUT, cwd=cwd, shell=True, text=True),
            lexer,
            strip_beginning=True
        ),
    ]).splitlines(True)

def code(path, start=None, end=None):
    pygments_cmd = ["pygmentize"]
    if path.endswith(".rlmeta"):
        pygments_cmd.extend(["-l", "rlmeta_lexer.py:RLMetaLexer", "-x"])
    else:
        pygments_cmd.extend(["-l", os.path.splitext(path)[1][1:]])
    pygments_cmd.extend(["-f", "html"])
    with open(path) as f:
        lines = f.read().splitlines(True)
    if start is not None:
        while lines and not re.search(start, lines[0]):
            lines.pop(0)
    if end is not None and lines:
        keep = [lines.pop(0)]
        while lines and not re.search(end, lines[0]):
            keep.append(lines.pop(0))
        lines = keep
    joined = "".join(lines)
    if start is not None or end is not None:
        joined = textwrap.dedent(joined)
    return subprocess.check_output(pygments_cmd, text=True, input=joined).splitlines(True)

def pygmentize(text, lexer, strip_beginning=False, strip_end=False):
    pygments_cmd = ["pygmentize"]
    if lexer == "rlmeta":
        pygments_cmd.extend(["-l", "rlmeta_lexer.py:RLMetaLexer", "-x"])
    else:
        pygments_cmd.extend(["-l", lexer])
    pygments_cmd.extend(["-f", "html"])
    html = subprocess.check_output(pygments_cmd, text=True, input=text)
    if strip_beginning:
        html = html.removeprefix('<div class="highlight"><pre>')
    if strip_end:
        html = html.removesuffix('\n</pre></div>\n')
    return html

for path in sys.argv[1:]:
    with open(path) as f:
        for line in f:
            for output in process_line(line):
                sys.stdout.write(output)
