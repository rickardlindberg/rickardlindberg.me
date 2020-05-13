#!/bin/bash

set -e

./compile.sh rlmeta.py > rlmeta1.py

./compile.sh rlmeta1.py > rlmeta2.py

./compile.sh rlmeta2.py > rlmeta3.py

echo "Test: Reproduces itself"
diff rlmeta2.py rlmeta3.py

echo "Test: Has its own support library embedded"
diff support.py <(python rlmeta3.py --support)

echo "Test: Error reporting string input"
echo "Grammar { foo = . " | python rlmeta3.py && false

mv rlmeta3.py rlmeta2.py

mv rlmeta2.py rlmeta1.py

mv rlmeta1.py rlmeta.py

echo "OK"
