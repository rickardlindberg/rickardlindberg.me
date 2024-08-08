#!/usr/bin/env python

import os
import re
import shutil
import subprocess

POSTS_BY_DATE = {}

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

def add_tag(headers, tag):
    if "tags" not in headers:
        tags = [tag]
    else:
        tags = headers["tags"].strip().split(",") + [tag]
    headers["tags"] = ",".join(tags) + "\n"

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
        header.pop("agdpp", None)
        header.pop("devlog", None)
        header.pop("layout", None)
        for x in header.keys():
            if x not in ["title", "date", "url", "tags"]:
                raise ValueError(x)
        assert data["url"].endswith("index.html\n")
        assert data["url"].startswith("/")
        export_dir = f"./export{os.path.dirname(data['url'])}"
        os.makedirs(export_dir)
        export_path = os.path.join(export_dir, "index.markdown")
        if header["date"] not in POSTS_BY_DATE:
            POSTS_BY_DATE[header["date"]] = []
        POSTS_BY_DATE[header["date"]].append(export_path)
        if "thought-of-the-day/2013" in export_path:
            add_tag(header, "totd1")
        if "thought-of-the-day/2014" in export_path:
            add_tag(header, "totd2")
        if "writing/reflections-on-programming" in export_path:
            add_tag(header, "rop")
        with open(export_path, "w") as f_out:
            f_out.write("---\n")
            for key, value in header.items():
                f_out.write(f"{key}: {value}")
            archive = f"http://archive.rickardlindberg.me{os.path.dirname(data['url'])}/"
            f_out.write("---\n")
            f_out.write("\n")
            f_out.write("This post has not yet been imported to my new blog.\n")
            f_out.write("\n")
            f_out.write(f"In the meantime, you can read it here: [{archive}]({archive}).\n")
        print(export_dir)

def patch_time(index, path):
    lines = []
    with open(path) as f:
        for line in f:
            if line.startswith("date:"):
                print(f"patch {index} {path}")
                lines.append(f"{line.strip()} 0{index}:00\n")
            else:
                lines.append(line)
    with open(path, "w") as f:
        f.write("".join(lines))

shutil.rmtree("./export", ignore_errors=True)
shutil.rmtree("export.zip", ignore_errors=True)
for (path, dirs, files) in os.walk("_site"):
    for file in files:
        if file.endswith(".html"):
            process_file(os.path.join(path, file))
for date, files in POSTS_BY_DATE.items():
    if len(files) > 1:
        print(date)
        for index, fi in enumerate(files):
            patch_time(index, fi)
subprocess.check_call("zip -r export.zip export/", shell=True)
print("Done!")
