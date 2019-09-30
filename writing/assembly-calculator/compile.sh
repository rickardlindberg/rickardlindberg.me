set -e

compile() {
    echo "import sys"
    echo "import pprint"
    python rlmeta/rlmeta.py --support
    cat "support.py"
    python rlmeta/rlmeta.py < parser.rlmeta
    python rlmeta/rlmeta.py < abstractcodegen.rlmeta
    python rlmeta/rlmeta.py < x86codegen.rlmeta
    python rlmeta/rlmeta.py < gas.rlmeta
    python rlmeta/rlmeta.py < assembler.rlmeta
    echo "main()"
}

python <(compile) "$@"
