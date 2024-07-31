#!/usr/bin/env python

import re
import sys

class Paragraph:

    def __init__(self):
        self.lines = []

    def add_markdown_line(self, line):
        self.lines.append(line)

    def flush(self):
        if self.lines:
            print("")
            print(" ".join(self.lines))
            self.lines.clear()

dividers = 0
paragraph = Paragraph()
for line in sys.stdin.read().splitlines():
    if line == "---":
        dividers += 1
    elif dividers == 1 and line.startswith("title:"):
        header = " ".join(line.split('"')[-2].split(" ")[1:])
        print(f"# {header}")
    elif dividers == 2 and line == "":
        paragraph.flush()
    elif dividers == 2:
        paragraph.add_markdown_line(
            re.sub(
                r"[(]/(.*?)/index.html[)]",
                r"(http://rickardlindberg.me/\1/)",
                re.sub(
                    r'src="/(.*?)"',
                    r'src="http://rickardlindberg.me/\1"',
                    line
                )
            )
        )
paragraph.flush()

print("")
print("<hr>Thank you for reading. Don't hesitate to hit reply and tell me your thoughts and comments. See you next month!")
