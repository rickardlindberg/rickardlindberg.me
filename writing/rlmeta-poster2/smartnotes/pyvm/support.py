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
    for path in sys.argv[1:]:
        with open(path) as f:
            sys.stdout.write(compile_chain(
                [(Parser, "file"), (CodeGenerator, "asts")],
                f.read()
            ))
