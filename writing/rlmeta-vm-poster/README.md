Old:

    $ time ./meta_compile.sh 
    OK

    real	0m0.782s
    user	0m0.677s
    sys	0m0.106s

    rick@x220 | ~/rickardlindberg.me/writing/rlmeta-vm
    $ wc -l parser.rlmeta codegenerator.rlmeta support.py compile.sh 
       53 parser.rlmeta
       74 codegenerator.rlmeta
      302 support.py
       45 compile.sh
      474 total

New:

    $ time ./meta_compile.sh 
    OK

    real	0m0.729s
    user	0m0.633s
    sys	0m0.096s

    $ wc -l parser.rlmeta codegenerator.rlmeta support.py compile.sh 
       52 parser.rlmeta
       73 codegenerator.rlmeta
      236 support.py
       42 compile.sh
      403 total

Getting closer to t-shirt programming.

Diff:

    meld . ../rlmeta-vm
