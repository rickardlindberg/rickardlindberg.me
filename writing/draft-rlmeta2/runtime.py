def join(items):
    return "".join(items)

def compile_grammar(grammar):
    parser = Parser()
    code_generator = CodeGenerator()
    return code_generator.run("ast", parser.run("grammar", grammar))

if __name__ == "__main__":
    sys.stdout.write(compile_grammar(sys.stdin.read()))
