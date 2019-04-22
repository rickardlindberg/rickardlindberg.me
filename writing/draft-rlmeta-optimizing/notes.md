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

8. try and/or again

    $ time ./compile.sh rlmeta.py > /dev/null

    real	0m0.592s
    user	0m0.560s
    sys		0m0.031s

    a little faster, but not abvoius

    $ python -m cProfile -s tottime rlmeta.py < parser.rlmeta
    ...
             508708 function calls (437696 primitive calls) in 0.380 seconds

       Ordered by: internal time

       ncalls  tottime  percall  cumtime  percall filename:lineno(function)
     4898/230    0.039    0.000    0.351    0.002 rlmeta.py:12(_or)
        15852    0.025    0.000    0.041    0.000 rlmeta.py:215(fail)
      10620/2    0.019    0.000    0.354    0.177 rlmeta.py:48(_match_rule)
      21301/2    0.018    0.000    0.354    0.177 rlmeta.py:21(_and)
        23516    0.018    0.000    0.078    0.000 rlmeta.py:247(next)
        19154    0.013    0.000    0.020    0.000 rlmeta.py:279(__init__)
       8323/1    0.012    0.000    0.184    0.184 rlmeta.py:93(_match_list)
        12847    0.010    0.000    0.024    0.000 rlmeta.py:290(_advance)
         6613    0.010    0.000    0.052    0.000 rlmeta.py:79(_match_charseq)

    less time spend in or and and

9. collapse and remove newlines/indents

    $ time ./compile.sh rlmeta.py > /dev/null

    real	0m0.565s
    user	0m0.530s
    sys		0m0.034s

    a little overall faster (a little less readable generated code)

    $ python -m cProfile -s tottime rlmeta.py < parser.rlmeta
    ...
             457174 function calls (396876 primitive calls) in 0.343 seconds

       Ordered by: internal time

       ncalls  tottime  percall  cumtime  percall filename:lineno(function)
     4518/230    0.039    0.000    0.343    0.001 rlmeta.py:12(_or)
        15769    0.024    0.000    0.040    0.000 rlmeta.py:215(fail)
      10240/2    0.019    0.000    0.347    0.174 rlmeta.py:48(_match_rule)
        23516    0.017    0.000    0.078    0.000 rlmeta.py:247(next)
      20921/2    0.017    0.000    0.347    0.174 rlmeta.py:21(_and)
        19154    0.014    0.000    0.020    0.000 rlmeta.py:279(__init__)
       8323/1    0.012    0.000    0.176    0.176 rlmeta.py:93(_match_list)
         6613    0.010    0.000    0.052    0.000 rlmeta.py:79(_match_charseq)
        12847    0.010    0.000    0.024    0.000 rlmeta.py:290(_advance)
        27805    0.010    0.000    0.010    0.000 rlmeta.py:239(__init__)
        17623    0.010    0.000    0.010    0.000 {method 'format' of 'str' objects}

10. implement match call rule to get rid of ors in codegenerator

    $ time ./compile.sh rlmeta.py > /dev/null

    real	0m0.406s
    user	0m0.373s
    sys		0m0.032s

    real performance gain!

    $ python -m cProfile -s tottime rlmeta.py < parser.rlmeta
    ...
             274963 function calls (238101 primitive calls) in 0.220 seconds

       Ordered by: internal time

       ncalls  tottime  percall  cumtime  percall filename:lineno(function)
      11129/2    0.021    0.000    0.215    0.107 rlmeta.py:48(_match_rule)
     4177/252    0.020    0.000    0.209    0.001 rlmeta.py:12(_or)
         8460    0.013    0.000    0.022    0.000 rlmeta.py:219(fail)
         7021    0.011    0.000    0.056    0.000 rlmeta.py:79(_match_charseq)
       9010/2    0.010    0.000    0.215    0.107 rlmeta.py:21(_and)
         9099    0.009    0.000    0.020    0.000 rlmeta.py:269(_advance)
        10540    0.009    0.000    0.039    0.000 rlmeta.py:251(next)
         9100    0.008    0.000    0.011    0.000 rlmeta.py:261(__init__)
        12801    0.006    0.000    0.006    0.000 {method 'format' of 'str' objects}
         1443    0.006    0.000    0.008    0.000 rlmeta.py:161(write)
        26070    0.005    0.000    0.005    0.000 rlmeta.py:266(position)
         8460    0.005    0.000    0.005    0.000 rlmeta.py:227(__init__)
        10908    0.005    0.000    0.005    0.000 rlmeta.py:243(__init__)

11. don't memoize code generator

    not really significant

12. optimize position

    $ time ./compile.sh rlmeta.py > /dev/null

    real	0m0.392s
    user	0m0.364s
    sys		0m0.026s

    slightly better

    $ python -m cProfile -s tottime rlmeta.py < parser.rlmeta
    ...
             274963 function calls (238101 primitive calls) in 0.220 seconds

       Ordered by: internal time

       ncalls  tottime  percall  cumtime  percall filename:lineno(function)
     4177/252    0.020    0.000    0.206    0.001 rlmeta.py:12(_or)
      11129/2    0.020    0.000    0.212    0.106 rlmeta.py:48(_match_rule)
         8460    0.014    0.000    0.022    0.000 rlmeta.py:219(fail)
         7021    0.012    0.000    0.057    0.000 rlmeta.py:79(_match_charseq)
       9010/2    0.010    0.000    0.212    0.106 rlmeta.py:21(_and)
         9099    0.009    0.000    0.021    0.000 rlmeta.py:269(_advance)
        10540    0.009    0.000    0.039    0.000 rlmeta.py:251(next)
         9100    0.008    0.000    0.011    0.000 rlmeta.py:261(__init__)
        12801    0.006    0.000    0.006    0.000 {method 'format' of 'str' objects}
         1443    0.006    0.000    0.009    0.000 rlmeta.py:161(write)

13. make fail messages lazy

    $ time ./compile.sh rlmeta.py > /dev/null

    real	0m0.370s
    user	0m0.336s
    sys		0m0.034s

    $ python -m cProfile -s tottime rlmeta.py < parser.rlmeta
    ...
             268387 function calls (231525 primitive calls) in 0.204 seconds

       Ordered by: internal time

       ncalls  tottime  percall  cumtime  percall filename:lineno(function)
     4177/252    0.020    0.000    0.194    0.001 rlmeta.py:12(_or)
      11129/2    0.019    0.000    0.199    0.100 rlmeta.py:48(_match_rule)
         8460    0.013    0.000    0.021    0.000 rlmeta.py:219(fail)
         7021    0.010    0.000    0.050    0.000 rlmeta.py:79(_match_charseq)
         9099    0.010    0.000    0.021    0.000 rlmeta.py:269(_advance)
       9010/2    0.009    0.000    0.199    0.100 rlmeta.py:21(_and)
        10540    0.008    0.000    0.038    0.000 rlmeta.py:251(next)
         9100    0.007    0.000    0.011    0.000 rlmeta.py:261(__init__)
         8460    0.006    0.000    0.006    0.000 rlmeta.py:227(__init__)
         1443    0.005    0.000    0.008    0.000 rlmeta.py:161(write)
        10908    0.004    0.000    0.004    0.000 rlmeta.py:243(__init__)
         8460    0.004    0.000    0.025    0.000 rlmeta.py:248(fail)
        10993    0.004    0.000    0.005    0.000 rlmeta.py:256(is_at_end)
        885/5    0.004    0.000    0.199    0.040 rlmeta.py:27(_star)
     3274/747    0.004    0.000    0.005    0.000 rlmeta.py:141(create)
        12616    0.003    0.000    0.003    0.000 {method 'write' of 'cStringIO.StringO' objects}
        17881    0.002    0.000    0.002    0.000 rlmeta.py:266(position)
         1802    0.002    0.000    0.014    0.000 rlmeta.py:59(_match_range)
         6225    0.002    0.000    0.002    0.000 {method 'format' of 'str' objects}

14. peek to make things a little faster

    $ time ./compile.sh rlmeta.py > /dev/null

    real	0m0.351s
    user	0m0.319s
    sys		0m0.031s

    $ python -m cProfile -s tottime rlmeta.py < parser.rlmeta
    ...
             248506 function calls (211644 primitive calls) in 0.187 seconds

       Ordered by: internal time

       ncalls  tottime  percall  cumtime  percall filename:lineno(function)
      11129/2    0.019    0.000    0.182    0.091 rlmeta.py:48(_match_rule)
     4177/252    0.018    0.000    0.177    0.001 rlmeta.py:12(_or)
         8460    0.013    0.000    0.022    0.000 rlmeta.py:221(fail)
         7021    0.010    0.000    0.037    0.000 rlmeta.py:79(_match_charseq)
       9010/2    0.009    0.000    0.182    0.091 rlmeta.py:21(_and)
         8460    0.006    0.000    0.006    0.000 rlmeta.py:229(__init__)
        10540    0.005    0.000    0.011    0.000 rlmeta.py:253(peek)
         1443    0.005    0.000    0.008    0.000 rlmeta.py:163(write)
        10993    0.004    0.000    0.005    0.000 rlmeta.py:258(is_at_end)
        885/5    0.004    0.000    0.182    0.036 rlmeta.py:27(_star)
         8460    0.004    0.000    0.025    0.000 rlmeta.py:250(fail)
     3274/747    0.003    0.000    0.005    0.000 rlmeta.py:143(create)
         1802    0.003    0.000    0.011    0.000 rlmeta.py:59(_match_range)
         2574    0.003    0.000    0.006    0.000 rlmeta.py:271(advance)
        17881    0.002    0.000    0.002    0.000 rlmeta.py:268(position)
        12616    0.002    0.000    0.002    0.000 {method 'write' of 'cStringIO.StringO' objects}
         2575    0.002    0.000    0.004    0.000 rlmeta.py:263(__init__)
         6225    0.002    0.000    0.002    0.000 {method 'format' of 'str' objects}
