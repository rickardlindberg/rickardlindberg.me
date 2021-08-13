if __name__ == "__main__":
    import sys
    for path in sys.argv[1:]:
        with open(path) as f:
            sys.stdout.write(compile_chain(
                [(Parser, "file"), (CodeGenerator, "asts")],
                f.read()
            ))
