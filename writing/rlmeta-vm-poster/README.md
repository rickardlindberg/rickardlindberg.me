Old:

  rick@x220 | ~/rickardlindberg.me/writing/rlmeta-memoize-failures/vm
  $ time ./meta_compile.sh 
  OK

  real	0m0.732s
  user	0m0.613s
  sys	0m0.119s

  rick@x220 | ~/rickardlindberg.me/writing/rlmeta-memoize-failures/vm
  $ wc -l parser.rlmeta codegenerator.rlmeta support.py compile.sh 
     53 parser.rlmeta
     74 codegenerator.rlmeta
    313 support.py
     45 compile.sh
    485 total

New:

  rick@x220 | ~/rickardlindberg.me/writing/rlmeta-vm-poster
  $ time ./meta_compile.sh 
  OK

  real	0m0.682s
  user	0m0.582s
  sys	0m0.100s

  rick@x220 | ~/rickardlindberg.me/writing/rlmeta-vm-poster
  $ wc -l parser.rlmeta codegenerator.rlmeta support.py compile.sh 
     52 parser.rlmeta
     65 codegenerator.rlmeta
    223 support.py
     52 compile.sh
    392 total

Getting closer to t-shirt programming.

Annoys me:

* Assembly hard to read (not sure how to improve it)
* Counter is incremented at match time, not at eval time
* New frequency analysis after instructions change

Diff from memoizing:

    meld ../rlmeta-memoize-failures/vm/ .

    * Assemble function in support
    * Replace builder with join/indent
    * Let compile.sh do error reporting (support only gives information)
    * Compact and modify formatting to better suit poster
    * Only one semantic action class
    * charseq -> and + match object
    * Remove string matching
