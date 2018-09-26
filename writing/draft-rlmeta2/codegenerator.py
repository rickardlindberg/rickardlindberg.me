class CodeGenerator(_RLMeta):

    def _rule_ast(self):
        return (lambda:
            self._or([
                # ["Grammar" .:x ast*:xs]    -> { 'class ' x '(_RLMeta):\n' > xs <                    }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("Grammar")
                                            ),
                                            (lambda:
                                                _vars.bind("x", self._any())
                                            ),
                                            (lambda:
                                                _vars.bind("xs", (lambda:
                                                    self._star((lambda:
                                                        self._match("ast")
                                                    ))
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "class ",
                                        _vars.lookup("x").eval(),
                                        "(_RLMeta):\n",
                                        _IndentBuilder(),
                                        _vars.lookup("xs").eval(),
                                        _DedentBuilder(),
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["Rule" .:x ast:y]         -> { '\ndef _rule_' x '(self):\n' > 'return ' y '()\n' < }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("Rule")
                                            ),
                                            (lambda:
                                                _vars.bind("x", self._any())
                                            ),
                                            (lambda:
                                                _vars.bind("y", (lambda:
                                                    self._match("ast")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "\ndef _rule_",
                                        _vars.lookup("x").eval(),
                                        "(self):\n",
                                        _IndentBuilder(),
                                        "return ",
                                        _vars.lookup("y").eval(),
                                        "()\n",
                                        _DedentBuilder(),
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["MatchAny"]               -> { 'self._any'                                         }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("MatchAny")
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "self._any",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["String" .:x]             -> { repr(x)                                             }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("String")
                                            ),
                                            (lambda:
                                                _vars.bind("x", self._any())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        repr(
                                            _vars.lookup("x").eval(),
                                        )
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["List" astList:x]         -> { x                                                   }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("List")
                                            ),
                                            (lambda:
                                                _vars.bind("x", (lambda:
                                                    self._match("astList")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        _vars.lookup("x").eval(),
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["ListBuilder" astItems:x] -> { '_Builder.create([' x '])'                          }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("ListBuilder")
                                            ),
                                            (lambda:
                                                _vars.bind("x", (lambda:
                                                    self._match("astItems")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "_Builder.create([",
                                        _vars.lookup("x").eval(),
                                        "])",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["IndentBuilder"]      -> { '_IndentBuilder()'                                  }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("IndentBuilder")
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "_IndentBuilder()",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["DedentBuilder"]      -> { '_DedentBuilder()'                                  }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("DedentBuilder")
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "_DedentBuilder()",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["FnCall" .:x astItems:y]  -> { x '(' y ')'                                         }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("FnCall")
                                            ),
                                            (lambda:
                                                _vars.bind("x", self._any())
                                            ),
                                            (lambda:
                                                _vars.bind("y", (lambda:
                                                    self._match("astItems")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        _vars.lookup("x").eval(),
                                        "(",
                                        _vars.lookup("y").eval(),
                                        ")",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["VarLookup" .:x]          -> { '_vars.lookup(' repr(x) ').eval()'                  }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("VarLookup")
                                            ),
                                            (lambda:
                                                _vars.bind("x", self._any())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "_vars.lookup(",
                                        repr(
                                            _vars.lookup("x").eval(),
                                        ),
                                        ").eval()",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # astFnBody:x                -> { '(lambda:\n' > x < '\n)' }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("astFnBody")
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "(lambda:\n",
                                        _vars.lookup("x").eval(),
                                        "\n)",
                                    ]))
                                ),
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
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("Or")
                                            ),
                                            (lambda:
                                                _vars.bind("x", (lambda:
                                                    self._match("astItems")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "self._or([",
                                        _vars.lookup("x").eval(),
                                        "])",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["Scope" ast:x]            -> { '(lambda _vars:\n' > x < '()\n)(_Vars())'           }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("Scope")
                                            ),
                                            (lambda:
                                                _vars.bind("x", (lambda:
                                                    self._match("ast")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "(lambda _vars:\n",
                                        _vars.lookup("x").eval(),
                                        "()\n)(_Vars())",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["And" astItems:x]         -> { 'self._and([' x '])'                                }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("And")
                                            ),
                                            (lambda:
                                                _vars.bind("x", (lambda:
                                                    self._match("astItems")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "self._and([",
                                        _vars.lookup("x").eval(),
                                        "])",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["Bind" .:x ast:y]         -> { '_vars.bind(' repr(x) ', ' y '())'                  }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("Bind")
                                            ),
                                            (lambda:
                                                _vars.bind("x", self._any())
                                            ),
                                            (lambda:
                                                _vars.bind("y", (lambda:
                                                    self._match("ast")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "_vars.bind(",
                                        repr(
                                            _vars.lookup("x").eval(),
                                        ),
                                        ", ",
                                        _vars.lookup("y").eval(),
                                        "())",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["Star" ast:x]             -> { 'self._star(' x ')'                                 }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("Star")
                                            ),
                                            (lambda:
                                                _vars.bind("x", (lambda:
                                                    self._match("ast")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "self._star(",
                                        _vars.lookup("x").eval(),
                                        ")",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["MatchNothing"]           -> { 'None'                                              }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("MatchNothing")
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "None",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["Not" ast:x]              -> { 'self._negative_lookahead(' x ')'                   }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("Not")
                                            ),
                                            (lambda:
                                                _vars.bind("x", (lambda:
                                                    self._match("ast")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "self._negative_lookahead(",
                                        _vars.lookup("x").eval(),
                                        ")",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["SemanticAction" ast:x]   -> { '_SemanticAction(lambda: ' x ')'                    }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("SemanticAction")
                                            ),
                                            (lambda:
                                                _vars.bind("x", (lambda:
                                                    self._match("ast")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "_SemanticAction(lambda: ",
                                        _vars.lookup("x").eval(),
                                        ")",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["Apply" .:x]              -> { 'self._match(' repr(x) ')'                          }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("Apply")
                                            ),
                                            (lambda:
                                                _vars.bind("x", self._any())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "self._match(",
                                        repr(
                                            _vars.lookup("x").eval()
                                        ),
                                        ")",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["MatchCharRange" .:x .:y] -> { 'self._match_range(' repr(x) ', ' repr(y) ')'       }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("MatchCharRange")
                                            ),
                                            (lambda:
                                                _vars.bind("x", self._any())
                                            ),
                                            (lambda:
                                                _vars.bind("y", self._any())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "self._match_range(",
                                        repr(
                                            _vars.lookup("x").eval()
                                        ),
                                        ", ",
                                        repr(
                                            _vars.lookup("y").eval()
                                        ),
                                        ")",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["MatchString" .:x]        -> { 'self._match_string(' repr(x) ')'                   }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("MatchString")
                                            ),
                                            (lambda:
                                                _vars.bind("x", self._any())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "self._match_string",
                                        repr(
                                            _vars.lookup("x").eval()
                                        ),
                                        ")",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["MatchCharseq" .:x]       -> { 'self._match_charseq(' repr(x) ')'                  }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("MatchCharseq")
                                            ),
                                            (lambda:
                                                _vars.bind("x", self._any())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "self._match_charseq",
                                        repr(
                                            _vars.lookup("x").eval()
                                        ),
                                        ")",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ["MatchList" ast:x]        -> { 'self._match_list(' x ')'                           }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("MatchList")
                                            ),
                                            (lambda:
                                                _vars.bind("x", (lambda:
                                                    self._match("ast")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "self._match_list",
                                        _vars.lookup("x").eval(),
                                        ")",
                                    ]))
                                ),
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
                                (lambda:
                                    _vars.bind("xs", (lambda:
                                        self._star((lambda:
                                            self._match("astItem")
                                        ))
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "\n",
                                        _IndentBuilder(),
                                        _vars.lookup("xs").eval(),
                                        _DedentBuilder(),
                                    ]))
                                ),
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
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("ast")
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        _vars.lookup("x").eval(),
                                        ",\n",
                                    ]))
                                ),
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
                                (lambda:
                                    _vars.bind("xs", (lambda:
                                        self._star((lambda:
                                            self._match("astListItem")
                                        ))
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "(",
                                        _vars.lookup("xs").eval(),
                                        "[])",
                                    ]))
                                ),
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
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string("ListItemSplice")
                                            ),
                                            (lambda:
                                                _vars.bind("x", (lambda:
                                                    self._match("ast")
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        _vars.lookup("x").eval(),
                                        "+",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                # ast:x                      -> { '[' x ']+'                                          }
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("ast")
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        "[",
                                        _vars.lookup("x").eval(),
                                        "]+",
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()
