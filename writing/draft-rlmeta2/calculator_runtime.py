from operator import add, mul
calculator = Calculator()
while True:
    line = raw_input("> ")
    print(str(calculator.run("expression", line)))
