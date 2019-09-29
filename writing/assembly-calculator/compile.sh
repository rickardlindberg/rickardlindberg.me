set -e

compile() {
    echo "import sys"
    echo "import pprint"
    python rlmeta/rlmeta.py --support
    cat "support.py"
    python rlmeta/rlmeta.py < parser.rlmeta
    python rlmeta/rlmeta.py < stackmachine.rlmeta
    python rlmeta/rlmeta.py < assembly.rlmeta
    python rlmeta/rlmeta.py < gnu.rlmeta
    python rlmeta/rlmeta.py < assembler.rlmeta
    echo "main()"
}

python <(compile) "$@"
