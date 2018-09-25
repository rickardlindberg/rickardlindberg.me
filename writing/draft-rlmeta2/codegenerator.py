class CodeGenerator(_RLMeta):

    def _rule_ast(self):
        return (lambda:
            self._or([
                # ["Grammar" .:x ast*:xs]    -> { 'class ' x '(_RLMeta):\n' > xs <                    }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["Rule" .:x ast:x]         -> { '\ndef _rule_' x '(self):\n' > 'return ' x '()\n' < }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["MatchAny"]               -> { 'self._any'                                         }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["String" .:x]             -> { repr(x)                                             }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["List" astList:x]         -> { x                                                   }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["ListBuilder" astItems:x] -> { '_Builder.create([' x '])'                          }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["IndentBuilder" .:x]      -> { '_IndentBuilder()'                                  }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["DedentBuilder" .:x]      -> { '_DedentBuilder()'                                  }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["FnCall" .:x astItems:y]  -> { x '(' y ')'                                         }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["VarLookup" .:x]          -> { '_vars.lookup(' repr(x) ').eval()'                  }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # astFnBody:x                -> { '(lambda:\n' > x < '\n)' }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_astFnBody(self):
        return (lambda:
            self._or([
                # ["Or" astItems:x]          -> { 'self._or([' x '])'                                 }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["Scope" ast:x]            -> { '(lambda _vars:\n' > x < '()\n)(_Vars())'           }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["And" astItems:x]         -> { 'self._and([' x '])'                                }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["Bind" .:x ast:y]         -> { '_vars.bind(' repr(x) ', ' y '())'                  }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["Star" ast:x]             -> { 'self._star(' x ')'                                 }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["MatchNothing"]           -> { 'None'                                              }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["Not" ast:x]              -> { 'self._negative_lookahead(' x ')'                   }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["SemanticAction" ast:x]   -> { '_SemanticAction(lambda: ' x ')'                    }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["Apply" .:x]              -> { 'self._match(' repr(x) ')'                          }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["MatchCharRange" .:x .:y] -> { 'self._match_range(' repr(x) ', ' repr(y) ')'       }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["MatchString" .:x]        -> { 'self._match_string(' repr(x) ')'                   }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["MatchCharseq" .:x]       -> { 'self._match_charseq(' repr(x) ')'                  }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
                # ["MatchList" ast:x]        -> { 'self._match_list(' x ')'                           }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_astItems(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            # astItem*:xs         -> { '\n' > xs <                                         }
                            self._and([
                                matchers,
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_astItem(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            # ast:x               -> { x ',\n'                                             }
                            self._and([
                                matchers,
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_astList(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            # astListItem*:xs     -> { '(' xs '[])'                                        }
                            self._and([
                                matchers,
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_astListItem(self):
        return (lambda:
            self._or([
                # ["ListItemSplice" ast:x]   -> {     x  '+'                                          }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers,
                            ])
                        )()
                    )(_Vars())
                ),
                # ast:x                      -> { '[' x ']+'                                          }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                matchers,
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

import parserfull
parser_ast = parserfull.Parser().run("grammar", open("parser.rlmeta").read())
import pprint
pprint.pprint(CodeGenerator().run("ast", parser_ast))
