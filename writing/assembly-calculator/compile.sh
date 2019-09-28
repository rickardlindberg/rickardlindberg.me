set -e

compile() {
    echo "import sys"
    echo "import pprint"
    rlmeta --support
    cat "support.py"
    rlmeta < parser.rlmeta
    echo "main()"
}

python <(compile)
