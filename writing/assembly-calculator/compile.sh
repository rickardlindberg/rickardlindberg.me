set -e

compile() {
    echo "import sys"
    echo "import pprint"
    rlmeta --support
    cat "support.py"
    rlmeta < parser.rlmeta
    rlmeta < stackmachine.rlmeta
    rlmeta < assembly.rlmeta
    rlmeta < gnu.rlmeta
    echo "main()"
}

python <(compile) "$@"
