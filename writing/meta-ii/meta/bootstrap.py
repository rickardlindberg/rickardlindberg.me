from subprocess import Popen, PIPE
import sys


def main():
    b = Bootstrapper()
    b.start("meta0.meta", "meta0.py")
    # Iteration 1: add support for groups in regexps
    b.make(
        "meta1.meta", "meta0.py",
        "meta1step.py", [
            "recognizes meta0",
            "generates compiler that supports groups in regexps",
        ]
    )
    b.make(
        "meta1.meta", "meta1step.py",
        "meta1.py", [
            "recognizes meta1",
            "supports groups in regexps"
        ]
    )
    # Iteration 2: extract verbatim sections
    b.make(
        "meta2step.meta", "meta1.py",
        "meta2step.py", [
            "generates compiler that reads verbatim"
        ]
    )
    b.make(
        "meta2.meta", "meta2step.py",
        "meta2.py", [
            "that reads verbatim"
        ]
    )
    # Iteration 3: add . output operator
    b.make(
        "meta3.meta", "meta2.py",
        "meta3step.py", [
            "compiler that recognizes . output operator"
        ]
    )
    b.make(
        "meta4.meta", "meta3step.py",
        "meta3.py", [
        ]
    )
    # Iteration 4: cleanup
    b.make(
        "meta5.meta", "meta3.py",
        "meta3cleanup1.py", [
        ]
    )
    b.make(
        "meta5.meta", "meta3cleanup1.py",
        "meta3cleanup2.py", [
        ]
    )


class Bootstrapper(object):

    def start(self, src, compiler):
        print("Start: {} / [{}]".format(src, compiler))
        self.check_meta(src, compiler)

    def make(self, src, compiler, out, out_desctiption):
        print("")
        print("Making: {} -> [{}] => {}".format(src, compiler, out))
        write(out, run_compiler(src, compiler))
        self.check_meta(src, out)
        for line in out_desctiption:
            print("  - {}".format(line))

    def check_meta(self, src, compiler):
        success, stdout, stderr = run(
            run_compiler(src, compiler, check_return=False),
            ["diff", compiler, "-"]
        )
        if success:
            print("  Meta: YES")
        else:
            print("  Meta: no")


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


def read(path):
    with open(path) as f:
        return f.read()


def write(path, content):
    with open(path, "w") as f:
        f.write(content)


if __name__ == "__main__":
    main()
