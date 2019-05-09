#!/bin/bash

set -e

cd "$(dirname "$0")"

./compile.sh rlmeta.py > rlmeta1.py

./compile.sh rlmeta1.py > rlmeta2.py

./compile.sh rlmeta2.py > rlmeta3.py

./compile.sh rlmeta3.py > rlmeta4.py

diff rlmeta3.py rlmeta4.py

diff support.py <(python rlmeta3.py --support)

mv rlmeta4.py rlmeta3.py

mv rlmeta3.py rlmeta2.py

mv rlmeta2.py rlmeta1.py

mv rlmeta1.py rlmeta.py

echo OK
