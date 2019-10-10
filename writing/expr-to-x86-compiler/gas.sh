set -e

cat | bash compile.sh parser acodegen xcodegen gas @expr.s

gcc driver.c expr.s -o expr

echo ""
echo "=V======= Compile & run ========"
echo ""

./expr
