from subprocess import Popen, PIPE
import os
import sys


def main():
    b = Bootstrapper()
    b.start(src("0"), pycompiler("0"))
    # Add identifier bindings
    b.make(src("1"), pycompiler("0"), pyout("1"), [
        "recognizes identifier bindings",
        "generates compilers that bind identifiers",
    ])
    b.make(src("2"), pycompiler("1"), pyout("2"), [
        "uses identifier binding",
    ])
    # Clean up uses of identifiers
    b.make(src("3"), pycompiler("2"), pyout("3"), [
        "cleans up identifier uses",
        "also recognizes slightly different identifiers (all can have numbers)",
    ])
    # Swap identifier bindings order
    b.make(src("4"), pycompiler("3"), pyout("4"), [
        "generates compilers that recognize swapped identifiers",
    ])
    b.make(src("5"), pycompiler("4"), pyout("5"), [
        "recognizes swapped identifiers",
    ])
    # Extract runtime from grammar
    b.make(src("6"), pycompiler("5"), pyout("6"), [
        "extracts runtime from grammar",
    ])
    # Support repetition
    b.make(src("7"), pycompiler("6"), pyout("7"), [
        "recognizes repetition",
    ])
    b.make(src("8"), pycompiler("7"), pyout("8"), [
        "use repetition",
    ])
    # Nicer handling of space
    b.make(src("9"), pycompiler("8"), pyout("9"), [
        "generates compilers that match space differently",
    ])
    b.make(src("10"), pycompiler("9"), pyout("10"), [
        "uses different space mechanisms",
    ])
    # Clean up
    b.make(src("11"), pycompiler("10"), pyout("11"), [
        "clean up",
    ])
    # Prelude
    b.make(src("12"), pycompiler("11"), pyout("12"), [
        "recognizes prelude",
        "generates compilers that uses prelude",
    ])
    b.make(src("13"), pycompiler("12"), pyout("13"), [
        "use prelude",
    ])
    b.make(src("14"), pycompiler("13"), pyout("14"), [
        "use space from prelude",
    ])
    # Return stuff
    b.make(src("15"), pycompiler("14"), pyout("15"), [
        "recognizes choice returns",
    ])
    b.make(src("16"), pycompiler("15"), pyout("16"), [
        "uses choice returns",
    ])
    # Extract constants
    b.make(src("17"), pycompiler("16"), pyout("17"), [
        "add support for constants",
    ])
    b.make(src("18"), pycompiler("17"), pyout("18"), [
        "use constants",
    ])
    b.make(src("18"), pycompiler("18"), pyout("19"), [
        "",
    ])
    # Copy constants verbatim
    b.make(src("19"), pycompiler("19"), pyout("20"), [
        "",
    ])
    b.make(src("19"), pycompiler("20"), pyout("21"), [
        "",
    ])
    b.make(src("19"), pycompiler("21"), pyout("22"), [
        "",
    ])
    # Extract runtime
    b.make(src("20"), pycompiler("22"), pyout("23"), [
        "",
    ])
    b.make(src("20"), pycompiler("23"), pyout("24"), [
        "",
    ])
    # Add . newline operator and clean up
    b.make(src("21"), pycompiler("24"), pyout("25"), [
        "",
    ])
    b.make(src("22"), pycompiler("25"), pyout("26"), [
        "",
    ])
    b.make(src("22"), pycompiler("26"), pyout("27"), [
        "",
    ])
    # Change match re syntax to slash
    b.make(src("23"), pycompiler("27"), pyout("28"), [
        "recognizes named constants",
    ])
    b.make(src("24"), pycompiler("28"), pyout("29"), [
        "recognizes slash strings",
    ])
    b.make(src("25"), pycompiler("29"), pyout("30"), [
        "use slash strings",
    ])
    # Literal string verbatim
    b.make(src("26"), pycompiler("30"), pyout("31"), [
        "",
    ])
    b.make(src("27"), pycompiler("31"), pyout("32"), [
        "",
    ])
    # Add EOF
    b.make(src("28"), pycompiler("32"), pyout("33"), [
        "recognize EOF",
    ])
    b.make(src("28"), pycompiler("33"), pyout("34"), [
        "",
    ])
    b.make(src("29"), pycompiler("34"), pyout("35"), [
        "use eof",
    ])
    # cleaner .space()
    b.make(src("30"), pycompiler("35"), pyout("36"), [
        "",
    ])
    b.make(src("30"), pycompiler("36"), pyout("37"), [
        "",
    ])
    # cleanup
    b.make(src("31"), pycompiler("37"), pyout("38"), [
        "",
    ])
    b.make(src("31"), pycompiler("38"), pyout("39"), [
        "",
    ])
    # Cleaner try/except handling
    b.make(src("32"), pycompiler("39"), pyout("40"), [
        "",
    ])
    b.make(src("32"), pycompiler("40"), pyout("41"), [
        "",
    ])
    # Cleaner compiler string code
    b.make(src("33"), pycompiler("41"), pyout("42"), [
        "",
    ])
    b.make(src("34"), pycompiler("42"), pyout("43"), [
        "use dollar string",
    ])
    # Generate labels
    b.make(src("35"), pycompiler("43"), pyout("44"), [
        "add support for labels",
    ])
    b.make(src("36"), pycompiler("44"), pyout("45"), [
        "use labels",
    ])
    b.make(src("36"), pycompiler("45"), pyout("46"), [
        "",
    ])
    # Support parenthesis (anonymous rules)
    b.make(src("37"), pycompiler("46"), pyout("47"), [
        "add support for parenthesis",
    ])
    b.make(src("38"), pycompiler("47"), pyout("48"), [
        "use parenthesis",
    ])
    # Prevent repetition of output since it will create infinite loop?
    b.make(src("39"), pycompiler("48"), pyout("49"), [
        "",
    ])
    # Get rid of re by using PEG
    b.make(src("40"), pycompiler("49"), pyout("50"), [
        "support naming everything",
    ])
    b.make(src("40"), pycompiler("50"), pyout("51"), [
        "",
    ])
    b.make(src("41"), pycompiler("51"), pyout("52"), [
        "add support for not",
    ])
    b.make(src("42"), pycompiler("52"), pyout("53"), [
        "use not",
    ])
    b.make(src("42"), pycompiler("53"), pyout("54"), [
        "",
    ])
    # Add literal matching (to replace simple re:s)
    b.make(src("43"), pycompiler("54"), pyout("55"), [
        "",
    ])
    b.make(src("44"), pycompiler("55"), pyout("56"), [
        "use literal matching",
    ])
    # Get rid of final re in RLMeta
    b.make(src("45"), pycompiler("56"), pyout("57"), [
        "",
    ])
    b.make(src("45"), pycompiler("57"), pyout("58"), [
        "",
    ])
    b.make(src("46"), pycompiler("58"), pyout("59"), [
        "",
    ])
    # Remove slash string
    b.make(src("47"), pycompiler("59"), pyout("60"), [
        "",
    ])
    b.make(src("47"), pycompiler("60"), pyout("61"), [
        "",
    ])
    b.make(src("48"), pycompiler("61"), pyout("62"), [
        "",
    ])
    # Move EOF to prelude
    b.make(src("49"), pycompiler("62"), pyout("63"), [
        "",
    ])
    b.make(src("49"), pycompiler("63"), pyout("64"), [
        "",
    ])
    b.make(src("50"), pycompiler("64"), pyout("65"), [
        "",
    ])
    b.make(src("50"), pycompiler("65"), pyout("66"), [
        "",
    ])
    # Conditions
    b.make(src("51"), pycompiler("66"), pyout("67"), [
        "add support for conditions",
    ])
    b.make(src("51"), pycompiler("67"), pyout("68"), [
        "",
    ])
    # Remove re completely
    b.make(src("52"), pycompiler("68"), pyout("69"), [
        "",
    ])
    b.make(src("52"), pycompiler("69"), pyout("70"), [
        "",
    ])
    # Clean up runtime
    b.make(src("53"), pycompiler("70"), pyout("71"), [
        "",
    ])
    b.make(src("53"), pycompiler("71"), pyout("72"), [
        "",
    ])
    b.make(src("53"), pycompiler("72"), pyout("73"), [
        "",
    ])
    # Better error handling
    b.make(src("54"), pycompiler("73"), pyout("74"), [
        "",
    ])
    b.make(src("54"), pycompiler("74"), pyout("75"), [
        "",
    ])
    # Semantic action rewrite
    b.make(src("55"), pycompiler("75"), pyout("76"), [
        "recognizes new syntax",
    ])
    b.make(src("56"), pycompiler("76"), pyout("77"), [
        "uses new syntax",
    ])
    b.make(src("56"), pycompiler("77"), pyout("78"), [
        "",
    ])
    b.make(src("56"), pycompiler("78"), pyout("79"), [
        "",
    ])
    # Support more escape sequences in strings
    b.make(src("57"), pycompiler("79"), pyout("80"), [
        "",
    ])
    b.make(src("58"), pycompiler("80"), pyout("81"), [
        "",
    ])
    # Character classes
    b.make(src("59"), pycompiler("80"), pyout("81"), [
        "",
    ])
    b.make(src("59"), pycompiler("81"), pyout("82"), [
        "",
    ])
    # Experiment with return
    b.make(src("60"), pycompiler("82"), pyout("83"), [
        "",
    ])
    b.make(src("60"), pycompiler("83"), pyout("84"), [
        "",
    ])
    # Rename EOF to end
    b.make(src("61"), pycompiler("84"), pyout("85"), [
        "Rename EOF to end (1)",
    ])
    b.make(src("62"), pycompiler("85"), pyout("86"), [
        "Rename EOF to end (2)",
    ])
    # Fancier return string formatting
    b.make(src("63"), pycompiler("86"), pyout("87"), [
        "",
    ])
    b.make(src("64"), pycompiler("87"), pyout("88"), [
        "",
    ])
    # More comprehensive escapes
    b.make(src("65"), pycompiler("88"), pyout("89"), [
        "",
    ])
    b.make(src("66"), pycompiler("89"), pyout("90"), [
        "",
    ])
    # Higher order rules: sepBy
    b.make(src("67"), pycompiler("90"), pyout("91"), [
        "",
    ])
    b.make(src("68"), pycompiler("91"), pyout("92"), [
        "",
    ])
    b.make(src("69"), pycompiler("91"), pyout("92"), [
        "",
    ])
    b.make(src("70"), pycompiler("92"), pyout("93"), [
        "",
    ])
    # Surround higher order function
    b.make(src("71"), pycompiler("93"), pyout("94"), [
        "",
    ])
    b.make(src("71"), pycompiler("94"), pyout("95"), [
        "",
    ])
    b.make(src("72"), pycompiler("95"), pyout("96"), [
        "",
    ])
    # Fix return format bug (hard coded to x)
    b.make(src("73"), pycompiler("96"), pyout("97"), [
        "",
    ])
    b.make(src("74"), pycompiler("97"), pyout("98"), [
        "",
    ])
    # Clean up with higher order rules
    b.make(src("75"), pycompiler("98"), pyout("99"), [
        "",
    ])
    b.make(src("75"), pycompiler("99"), pyout("100"), [
        "",
    ])
    # Clean up
    b.make(src("76"), pycompiler("100"), pyout("101"), [
        "Get rid of _choices list",
    ])
    b.make(src("76"), pycompiler("101"), pyout("102"), [
        "",
    ])
    # Clean up semantic action context
    b.make(src("77"), pycompiler("102"), pyout("103"), [
        "",
    ])
    b.make(src("77"), pycompiler("103"), pyout("104"), [
        "",
    ])
    # TODO: Remove need for @ in dollar string output
    # TODO: Memoize results? Speed up PEG & more elegant solution?
    # TODO: Split parsing and code generation?
    # TODO: Add default start rule syntax? Default to first defined?
    # TODO: Clean up duplication in runtime
    # TODO: Better error handling
    # TODO: Use only # for labels
    # TODO: Make match, label, ect "private"
    # TODO: Add _and to PEG (How to handle assignments?)
    # TODO: Handle latest exception at raise point instead of catch point?
    # TODO: Make constans semantic actions that just output their value
    # TODO: Not all buildins return sematic actions (_not)


class Bootstrapper(object):

    def start(self, src, compiler):
        print("{} / [{}]".format(src, compiler).center(80))
        self.check_meta(src, compiler)
        self._last_source = src

    def make(self, src, compiler, out, out_desctiption):
        print("-"*79)
        print("{} -> [{}] => {}".format(src, compiler, out).center(79))
        print(" Source diff:")
        success, stdout, stderr = run("", ["diff", self._last_source, src])
        for line in stdout.splitlines():
            print("   {}".format(line))
        print(" Creates compiler that:")
        for line in out_desctiption:
            print("   - {}".format(line))
        write(out, run_compiler(src, compiler))
        self.check_meta(src, out)
        print(" Compiler diff:")
        print("   - diff {} {}".format(compiler, out))
        print("   - meld {} {}".format(compiler, out))
        self._last_source = src

    def check_meta(self, src, compiler):
        success, stdout, stderr = run(
            run_compiler(src, compiler, check_return=False),
            ["diff", compiler, "-"]
        )
        if success:
            symlink(src, "rlmeta.rlmeta")
            symlink(compiler, "rlmeta.py")
            print(" Is meta compiler: YES")
        else:
            print(" Is meta compiler: no")


def src(version):
    return "rlmeta{}.rlmeta".format(version)


def pycompiler(version):
    return "rlmeta{}.py".format(version)


def pyout(version):
    return "rlmeta{}.py".format(version)


def run_compiler(src, compiler, check_return=True):
    success, stdout, stderr = run(read(src), ["python", compiler])
    if check_return and not success:
        sys.exit(
            "ERROR: {} -> [{}] failed:\n{}".format(src, compiler, stderr)
        )
    return stdout


def run(stdin, cmd):
    p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate(stdin)
    return p.returncode == 0, stdout, stderr


def symlink(src, target):
    if os.path.islink(target):
        os.remove(target)
    os.symlink(src, target)


def read(path):
    with open(path) as f:
        return f.read()


def write(path, content):
    with open(path, "w") as f:
        f.write(content)


if __name__ == "__main__":
    main()
