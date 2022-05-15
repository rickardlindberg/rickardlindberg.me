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
        lines.append("*This is a draft.*\n")
    return lines

if __name__ == "__main__":
    import sys
    import datetime
    for path in sys.argv[1:]:
        with open(path+".template") as source:
            with open(path, "w") as dest:
                dest.write(compile_chain(
                    [(Processor, "file")],
                    source.read(),
                    {"pre": pre}
                ))
