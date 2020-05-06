Annoys me:

* Assembly hard to read (not sure how to improve it)
* Counter is incremented at match time, not at eval time (leave it for now)

Diff from memoizing:

    meld ../rlmeta-memoize-failures/vm/ .

    * Assemble function in support
    * Replace builder with join/indent
    * Let compile.sh do error reporting (support only gives information)
    * Compact and modify formatting to better suit poster
    * Only one semantic action class
    * charseq -> and + match object
    * Remove string matching

$ (cat support.py; G='Counter { count = .*:xs -> { "count = " len(xs) } }'; echo "$G" | python rlmeta.py) > g.py && python -c 'from g import *; print(Counter().run("count", "Hello"))'
count = 5
