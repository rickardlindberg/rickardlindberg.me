#!/bin/bash

set -e

cd "$(dirname "$0")"

./compile.sh rlmeta.py > rlmeta1.py

./compile.sh rlmeta1.py > rlmeta2.py

diff rlmeta1.py rlmeta2.py

diff support.py <(python rlmeta2.py --support)

mv rlmeta2.py rlmeta1.py

mv rlmeta1.py rlmeta.py

echo OK
