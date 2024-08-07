#!/usr/bin/env python

import subprocess
import sys
import textwrap
import os

def process_file(path):
    with open(path) as f:
        marker_count = 0
        while True:
            line = f.readline()
            sys.stdout.write(line)
            if line.strip() == "---":
                marker_count += 1
            if marker_count == 2:
                break
        sys.stdout.write(process_converted_to_markdown(subprocess.check_output([
            "pandoc", "-f", "html", "-t", "markdown"
        ], text=True, input=f.read())))

def process_converted_to_markdown(text):
    stack = LineStack()
    for line in text.splitlines(True):
        if line.startswith("::: {"):
            if "{.rliterate-image}" in line:
                stack.push("image")
            elif "{.rliterate-image-text}" in line:
                stack.push("image-text")
            elif "{.rliterate-code}" in line:
                stack.push("code")
            elif "{.rliterate-code-header}" in line:
                stack.push("code-header")
            elif "{.rliterate-code-body}" in line:
                stack.push("code-body")
            elif "{.jumbotron}" in line:
                stack.push("jumbotron")
            else:
                raise ValueError(line)
        elif line.strip() == ":::":
            stack.pop()
        else:
            stack.append(line)
    return stack.export()

class LineStack:

    def __init__(self):
        self.stack = []
        self.lines = []
        self.name = None

    def push(self, name):
        self.stack.append((self.lines, self.name))
        self.lines = []
        self.name = name
        if name == "code":
            self.syntax = "text"

    def pop(self):
        lines = self.lines
        name = self.name
        self.lines, self.name = self.stack.pop(-1)
        if name == "image-text":
            self.lines.append("<!-- image text -->\n")
            self.lines.append("<center>\n")
            self.lines.extend(lines)
            self.lines.append("</center>\n")
        elif name == "code-header":
            self.lines.append("```\n")
            self.lines.extend(textwrap.dedent("".join(lines)))
            self.lines.append("```\n")
            self.syntax = os.path.splitext(lines[0].split("1.  ")[1].strip())[1][1:]
        elif name == "code-body":
            self.lines.append(f"```{self.syntax}\n")
            self.lines.extend(textwrap.dedent("".join(lines)))
            self.lines.append("```\n")
        elif name == "jumbotron":
            self.lines.append(f"<hr>\n\n")
            self.lines.extend("".join(lines))
            self.lines.append(f"\n<hr>\n")
        else:
            self.lines.extend(lines)

    def append(self, line):
        self.lines.append(line)

    def export(self):
        assert self.stack == []
        return "".join(self.lines)


for path in sys.argv[1:]:
    process_file(path)
