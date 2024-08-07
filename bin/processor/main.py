def pre(header):
    lines = []
    lines.append("---\n")
    if "draft" in header.get("tags", []):
        header["title"] = repr(f"DRAFT: {header['title']}")
        header["date"] = datetime.datetime.now().isoformat()[:10]
    for key, value in header.items():
        if isinstance(value, list):
            lines.append(f"{key}: {','.join(value)}\n")
        else:
            lines.append(f"{key}: {value}\n")
    lines.append("---\n")
    if "draft" in header.get("tags", []):
        lines.append("\n")
        lines.append("**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**\n")
    return lines

def code(path, start=None, end=None):
    pygments_cmd = ["pygmentize"]
    language = "text"
    if path.endswith(".rlmeta"):
        pygments_cmd.extend(["-l", "rlmeta_lexer.py:RLMetaLexer", "-x"])
    else:
        language = os.path.splitext(path)[1][1:]
        pygments_cmd.extend(["-l", language])
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
    assert joined.endswith("\n")
    return f"```{language}\n{joined}```\n"

def output(title, text, syntax="text"):
    pygments_cmd = ["pygmentize"]
    pygments_cmd.extend(["-f", "html"])
    pygments_cmd.extend(["-l", syntax])
    joined = text
    assert joined.endswith("\n")
    title_code = []
    if title.strip():
        title_text = f"`{title.strip()}`:\n\n"
    else:
        title_text = ""
    return f"{title_text}```{syntax}\n{joined}```\n"

if __name__ == "__main__":
    import sys
    import datetime
    import os
    import subprocess
    for path in sys.argv[1:]:
        if path == "index.template.markdown":
            source_path = path
            destination_path = "index.markdown"
        else:
            source_path = path+".template"
            destination_path = path
        with open(source_path) as source:
            with open(destination_path, "w") as dest:
                dest.write(compile_chain(
                    [(Processor, "file")],
                    source.read(),
                    {"pre": pre, "code": code, "output": output}
                ))
