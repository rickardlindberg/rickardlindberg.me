from operator import add, mul

if __name__ == "__main__":
    calculator = Calculator()
    while True:
        line = raw_input("> ")
        print(calculator.run("expression", line))
