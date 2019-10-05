set -e

echo "$1" | bash compile.sh parser acodegen xcodegen gas @expr.s

gcc driver.c expr.s -o expr

echo ""
echo "=V=============================="
echo ""

echo "$1 = $(./expr)"
