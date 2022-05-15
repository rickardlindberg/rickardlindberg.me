def pre(header):
    lines = []
    lines.append("---\n")
    if "draft" in header.get("tags", []):
        header["title"] = f"DRAFT: {header['title']}"
        header["date"] = datetime.datetime.now().isoformat()[:10] + "\n"
        for key, value in header.items():
            if isinstance(value, list):
                lines.append(f"{key}: {','.join(value)}\n")
            else:
                lines.append(f"{key}: {value}")
    lines.append("---\n")
    if "draft" in header.get("tags", []):
        lines.append("\n")
        lines.append("**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**\n")
    return lines

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

if __name__ == "__main__":
    import sys
    import datetime
    import os
    import subprocess
    for path in sys.argv[1:]:
        with open(path+".template") as source:
            with open(path, "w") as dest:
                dest.write(compile_chain(
                    [(Processor, "file")],
                    source.read(),
                    {"pre": pre, "code": code}
                ))
