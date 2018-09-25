class Parser(_RLMeta):

    def _rule_grammar(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("name")
                                    )())
                                ),
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("{")
                                ),
                                (lambda:
                                    _vars.bind("ys", (lambda:
                                        self._star((lambda:
                                            self._match("rule")
                                        ))
                                    )())
                                ),
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("}")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["Grammar"]+[_vars.lookup("x").eval()]+_vars.lookup("ys").eval()+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_rule(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("name")
                                    )())
                                ),
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("=")
                                ),
                                (lambda:
                                    _vars.bind("y", (lambda:
                                        self._match("choices")
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["Rule"]+[_vars.lookup("x").eval()]+[_vars.lookup("y").eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_choices(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._or([
                                        (lambda:
                                            self._or([
                                                (lambda:
                                                    (lambda _vars:
                                                        (lambda:
                                                            self._and([
                                                                (lambda:
                                                                    self._match("space")
                                                                ),
                                                                (lambda:
                                                                    self._match_charseq("|")
                                                                )
                                                            ])
                                                        )()
                                                    )(_Vars())
                                                ),
                                            ])
                                        ),
                                        (lambda:
                                            None
                                        ),
                                    ])
                                ),
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("sequence")
                                    )())
                                ),
                                (lambda:
                                    _vars.bind("xs", (lambda:
                                        self._star((lambda:
                                            self._or([
                                                (lambda:
                                                    (lambda _vars:
                                                        (lambda:
                                                            self._and([
                                                                (lambda:
                                                                    self._match("space")
                                                                ),
                                                                (lambda:
                                                                    self._match_charseq("|")
                                                                ),
                                                                (lambda:
                                                                    self._match("sequence")
                                                                ),
                                                            ])
                                                        )()
                                                    )(_Vars())
                                                )
                                            ])
                                        ))
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["Or"]+[_vars.lookup("x").eval()]+_vars.lookup("xs").eval()+[]))
                                )
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_sequence(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("expr")
                                    )())
                                ),
                                (lambda:
                                    _vars.bind("xs", (lambda:
                                        self._star((lambda:
                                            self._match("expr")
                                        ))
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["Scope"]+(["And"]+[_vars.lookup("x").eval()]+_vars.lookup("xs").eval()+[])+[]))
                                )
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_expr(self):
        return self._match_charseq("x")

    def _rule_name(self):
        return self._match_charseq("n")

    def _rule_space(self):
        return self._match_charseq(" ")


import pprint
pprint.pprint(Parser().run("grammar", "n {n = |x }"))
