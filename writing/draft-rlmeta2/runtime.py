def join(items):
    return "".join(items)

parser = Parser()
code_generator = CodeGenerator()
sys.stdout.write(code_generator.run("ast", parser.run("grammar", sys.stdin.read())))
