#!/usr/bin/env python3

import sys
import subprocess

def process_line(line):
    if line.startswith("$:shell:"):
        return shell(*line.strip().split(":")[2:])
    elif line.startswith("$#shell#"):
        return shell(*line.strip().split("#")[2:])
    elif line.startswith("$:code:"):
        return code(*line.strip().split(":")[2:])
    else:
        return [line]

def shell(cwd, cmd, lexer="text"):
    return "".join([
        pygmentize("$ ", "text", strip_end=True),
        pygmentize(cmd, "bash", strip_beginning=True, strip_end=True),
        '\n',
        pygmentize(
            subprocess.check_output(cmd, cwd=cwd, shell=True, text=True),
            lexer,
            strip_beginning=True
        ),
    ]).splitlines(True)

def code(path):
    pygments_cmd = ["pygmentize"]
    if path.endswith(".rlmeta"):
        pygments_cmd.extend(["-l", "rlmeta_lexer.py:RLMetaLexer", "-x"])
    pygments_cmd.extend(["-f", "html"])
    pygments_cmd.append(path)
    return subprocess.check_output(pygments_cmd, text=True).splitlines(True)

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
