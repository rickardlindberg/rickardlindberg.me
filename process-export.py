#!/usr/bin/env python

import os
import shutil

def process_file(path):
    with open(path) as f:
        if f.readline().strip() == "---EXPORT---":
            process_export_data(f)

def process_export_data(f):
    title = f.readline()
    url = f.readline()
    path = f.readline()
    date = f.readline()
    process_export_data_parsed({
        "title": title,
        "url": url,
        "path": path,
        "date": date,
    })

def process_export_data_parsed(data):
    with open(data["path"].strip()) as f:
        assert f.readline().strip()
        header = {}
        while True:
            line = f.readline()
            if line.strip() == "---":
                break
            else:
                name, value = line.split(": ", 1)
                header[name] = value
        if "date" not in header:
            header["date"] = data["date"]
        header["url"] = f"{os.path.dirname(data['url'])}/\n"
        assert data["url"].endswith("index.html\n")
        assert data["url"].startswith("/")
        export_dir = f"./export{os.path.dirname(data['url'])}"
        os.makedirs(export_dir)
        with open(os.path.join(export_dir, "index.markdown"), "w") as f_out:
            f_out.write("---\n")
            for key, value in header.items():
                f_out.write(f"{key}: {value}")
            f_out.write("---\n")
            f_out.write(f.read())
        print(export_dir)

shutil.rmtree("./export", ignore_errors=True)
for (path, dirs, files) in os.walk("_site"):
    for file in files:
        if file.endswith(".html"):
            process_file(os.path.join(path, file))
print("Done!")
