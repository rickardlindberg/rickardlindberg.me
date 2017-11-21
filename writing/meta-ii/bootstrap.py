from subprocess import Popen, PIPE
import sys


def main():
    b = Bootstrapper()
    b.start("meta0.meta0", "meta0.py")
    # Iteration 1: add support for groups in regexps
    b.make(
        "meta1.meta0", "meta0.py",
        "meta1step.py", [
            "recognizes meta0",
            "generates compiler that supports groups in regexps",
        ]
    )
    b.make(
        "meta1.meta0", "meta1step.py",
        "meta1.py", [
            "recognizes meta1",
            "supports groups in regexps"
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
