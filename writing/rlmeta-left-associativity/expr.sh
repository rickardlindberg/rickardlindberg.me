compile() {
    echo "import sys"
    echo "import pprint"
    rlmeta --support
    rlmeta < "$1"
    cat "support.py"
    cat "$2"
    echo "makeAdd = lambda: add"
    echo "makeSub = lambda: sub"
    echo "makeMul = lambda: mul"
    echo "makeDiv = lambda: div"
    echo "makePow = lambda: pow"
    echo "try:"
    echo "    for expr in sys.stdin.read().splitlines():"
    echo "        pprint.pprint(Calculator().run('expr', expr), width=20)"
    echo "except _MatchError as e:"
    echo "    sys.stderr.write(e.describe())"
}

(
    while read -e expr; do
        echo "$expr" | python <(compile "$1" "ast.py")
        echo "=>"
        echo "$expr" | python <(compile "$1" "eval.py")
        echo ""
    done
) | head -n-1
