class List(list):
    def __call__(self, item):
        self.append(item)

class Dict(dict):
    def __call__(self, *args):
        if len(args) == 1:
            return self[args[0]]
        else:
            self[args[0]] = args[1]

if __name__ == "__main__":
    import sys
    import pprint
    for path in sys.argv[1:]:
        with open(path) as f:
            try:
                ast = Parser().run("vm", f.read())
                code = CodeGenerator().run("ast", [ast])
                sys.stdout.write(code)
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
