#!/usr/bin/env python

import base64
import os
import re

def process_file(path):
    images = []
    def replace(x):
        images.append(None)
        name = f"image{len(images)}.png"
        with open(os.path.join(os.path.dirname(path), name), "wb") as f:
            f.write(base64.b64decode(x.group(1)))
        return f'"{name}"'
    with open(path) as f:
        text = f.read()
        new_text = re.sub(r'"data:image/png;base64,(.*?)"', replace, text, flags=re.DOTALL)
        if text != new_text:
            with open(path, "w") as f:
                f.write(new_text)
            print(path)

for (path, dirs, files) in os.walk("."):
    for file in files:
        if file.endswith(".html"):
            process_file(os.path.join(path, file))
print("Done!")
