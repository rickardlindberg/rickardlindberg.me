def compile_grammar(grammar):
    return CodeGenerator().run(
        "ast",
        [Parser().run("grammar", grammar)]
    )

if __name__ == "__main__":
    import sys
    import pprint
    def read(path):
        if path == "-":
            return sys.stdin.read()
        with open(path) as f:
            return f.read()
    args = sys.argv[1:] or ["--compile", "-"]
    while args:
        command = args.pop(0)
        if command == "--support":
            sys.stdout.write(SUPPORT)
        elif command == "--copy":
            sys.stdout.write(read(args.pop(0)))
        elif command == "--embed":
            sys.stdout.write("{} = {}\n".format(
                args.pop(0),
                repr(read(args.pop(0)))
            ))
        elif command == "--compile":
            try:
                sys.stdout.write(compile_grammar(read(args.pop(0))))
            except MatchError as e:
                stream = e.stream
                for pos in e.pos[:-1]:
                    stream = stream[pos]
                pos = e.pos[-1]
                MARKER = "\033[0;31m<ERROR POSITION>\033[0m"
                if isinstance(stream, str):
                    stream_string = stream[:pos] + MARKER + stream[pos:]
                else:
                    stream_string = pprint.pformat(stream)
                sys.exit("ERROR: {}\nPOSITION: {}\nSTREAM:\n{}".format(
                    e.message,
                    pos,
                    indent(stream_string)
                ))
        else:
            sys.exit("ERROR: Unknown command '{}'".format(command))
