from subprocess import Popen, PIPE
import os
import sys


def main():
    b = Bootstrapper()
    b.start(src("0"), pycompiler("0"))
    # Iteration 1: add support for groups in regexps
    b.make(src("1"), pycompiler("0"), pyout("1step"), [
        "recognizes meta0",
        "generates pycompiler that supports groups in regexps",
    ])
    b.make(src("1"), pycompiler("1step"), pyout("1"), [
        "recognizes meta1",
        "supports groups in regexps"
    ])
    # Iteration 2: extract verbatim sections
    b.make(src("2step"), pycompiler("1"), pyout("2step"), [
        "generates pycompiler that reads verbatim"
    ])
    b.make(src("2"), pycompiler("2step"), pyout("2"), [
        "that reads verbatim"
    ])
    # Iteration 3: add . output operator
    b.make(src("3"), pycompiler("2"), pyout("3step"), [
        "pycompiler that recognizes . output operator"
    ])
    b.make(src("4"), pycompiler("3step"), pyout("3"), [
    ])
    # Iteration 4: cleanup
    b.make(src("5"), pycompiler("3"), pyout("3cleanup1"), [
    ])
    b.make(src("5"), pycompiler("3cleanup1"), pyout("3cleanup2"), [
    ])


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
            symlink(src, "meta.meta")
            symlink(compiler, "meta.py")
            print("  Meta: YES")
        else:
            print("  Meta: no")


def src(version):
    return "meta{}.meta".format(version)


def pycompiler(version):
    return "meta{}.py".format(version)


def pyout(version):
    return "meta{}.py".format(version)


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
