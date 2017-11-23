from subprocess import Popen, PIPE
import os
import sys


def main():
    b = Bootstrapper()
    b.start(src("0"), pycompiler("0"))
    b.make(src("1"), pycompiler("0"), pyout("1"), [
        "recognizes identifier bindings",
        "generates compilers that bind identifiers",
    ])
    b.make(src("2"), pycompiler("1"), pyout("2"), [
        "uses identifier binding",
    ])
    b.make(src("3"), pycompiler("2"), pyout("3"), [
        "cleans up identifier uses",
        "also recognizes slightly different identifiers (all can have numbers)",
    ])


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
