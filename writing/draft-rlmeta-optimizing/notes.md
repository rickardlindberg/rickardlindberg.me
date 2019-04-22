1. measure base

    $ time ./compile.sh rlmeta.py > /dev/null

    real	0m0.756s
    user	0m0.701s
    sys		0m0.054s

2. what takes time?

    $ time python rlmeta.py < parser.rlmeta > /dev/null

    real	0m0.380s
    user	0m0.363s
    sys		0m0.015s

    $ time python rlmeta.py < codegenerator.rlmeta > /dev/null

    real	0m0.351s
    user	0m0.331s
    sys		0m0.019s

3. more specifically?

    $ python -m cProfile -s tottime rlmeta.py < parser.rlmeta
    ...
             513780 function calls (430760 primitive calls) in 0.476 seconds

   Ordered by: internal time

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
   8182/2    0.053    0.000    0.427    0.213 rlmeta.py:7(_or)
    22758    0.040    0.000    0.106    0.000 rlmeta.py:234(next)
    17411    0.027    0.000    0.046    0.000 rlmeta.py:203(fail)
  26658/2    0.023    0.000    0.427    0.213 rlmeta.py:16(_and)
  10730/2    0.020    0.000    0.427    0.213 rlmeta.py:43(_match_rule)
     2177    0.019    0.000    0.019    0.000 rlmeta.py:147(write)
        1    0.013    0.013    0.013    0.013 {method 'write' of 'file' objects}
    ...

    $ python -m cProfile -s tottime rlmeta.py < codegenerator.rlmeta
    ...
         450885 function calls (377441 primitive calls) in 0.433 seconds

   Ordered by: internal time

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
   8152/2    0.052    0.000    0.396    0.198 rlmeta.py:7(_or)
    19754    0.046    0.000    0.097    0.000 rlmeta.py:234(next)
    15838    0.026    0.000    0.049    0.000 rlmeta.py:203(fail)
  23478/2    0.020    0.000    0.396    0.198 rlmeta.py:16(_and)
   9883/2    0.018    0.000    0.396    0.198 rlmeta.py:43(_match_rule)
    15838    0.016    0.000    0.016    0.000 rlmeta.py:211(__init__)
        1    0.012    0.012    0.012    0.012 {method 'write' of 'file' objects}

    ...

4. conclusions

    * The two different grammars have similar performance characteristics
    * Areas where performance can be improved
        * _or: reduce number of ors
        * next: avoid slicing
        * fail: not sure what to do except avoid fails (with less _ors)
        * _and: not sure what to do except avoid _ands
        * _match_rule: use better lookup of function name
        * write: faster string concatenation

5. try reducing ands and ors

    $ git diff codegenerator.rlmeta
    diff --git a/writing/draft-rlmeta-optimizing/codegenerator.rlmeta b/writing/draft-rlmeta-optimizing/codegenerator.rlmeta
    index 293e013..2177693 100644
    --- a/writing/draft-rlmeta-optimizing/codegenerator.rlmeta
    +++ b/writing/draft-rlmeta-optimizing/codegenerator.rlmeta
    @@ -10,6 +10,8 @@ CodeGenerator {
         | ["DedentBuilder"]         -> { "_DedentBuilder()"                                  }
         | ["FnCall" .:x astItems:y] -> { x "(" y ")"                                         }
         | ["VarLookup" .:x]         -> { "_vars.lookup(" repr(x) ").eval()"                  }
    +    | ["Or" ast:x]              -> x
    +    | ["And" ast:x]             -> x
         | astFnBody:x               -> { "(lambda:\n" > x < "\n)" }
       astFnBody =
         | ["Or" astItems:x]         -> { "self._or([" x "])"                                 }

    Did not seem to have any effect.

    Why? Codegenerator becomes bigger which makes it slower.

6. try avoid slicing input

    $ time ./compile.sh rlmeta.py > /dev/null

    real	0m0.608s
    user	0m0.585s
    sys		0m0.022s

    from ~750 -> ~600

    $ python -m cProfile -s tottime rlmeta.py < parser.rlmeta
    ...
             513780 function calls (430760 primitive calls) in 0.415 seconds

       Ordered by: internal time

       ncalls  tottime  percall  cumtime  percall filename:lineno(function)
       8182/2    0.042    0.000    0.366    0.183 rlmeta.py:7(_or)
        17411    0.026    0.000    0.044    0.000 rlmeta.py:203(fail)
      26658/2    0.021    0.000    0.366    0.183 rlmeta.py:16(_and)
      10730/2    0.019    0.000    0.366    0.183 rlmeta.py:43(_match_rule)
         2177    0.019    0.000    0.019    0.000 rlmeta.py:147(write)
        22758    0.017    0.000    0.077    0.000 rlmeta.py:235(next)
            1    0.014    0.014    0.014    0.014 {method 'write' of 'file' objects}

    next has moved down the list ~0.40 -> ~0.17

7. faster string concatenation

    $ time ./compile.sh rlmeta.py > /dev/null

    real	0m0.603s
    user	0m0.564s
    sys		0m0.038s

    roughly the same time

    $ python -m cProfile -s tottime rlmeta.py < parser.rlmeta
             527910 function calls (444890 primitive calls) in 0.397 seconds

       Ordered by: internal time

       ncalls  tottime  percall  cumtime  percall filename:lineno(function)
       8182/2    0.042    0.000    0.370    0.185 rlmeta.py:12(_or)
        17411    0.026    0.000    0.045    0.000 rlmeta.py:215(fail)
      26658/2    0.021    0.000    0.370    0.185 rlmeta.py:21(_and)
      10730/2    0.019    0.000    0.370    0.185 rlmeta.py:48(_match_rule)
        22758    0.017    0.000    0.078    0.000 rlmeta.py:247(next)
        17408    0.012    0.000    0.018    0.000 rlmeta.py:279(__init__)
        ...
         2177    0.006    0.000    0.009    0.000 rlmeta.py:157(write)

    but much less time spent in write
