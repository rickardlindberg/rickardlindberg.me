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
                                    self._match_charsec("{")
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
                                    self._match_charsec("}")
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
                                    self._match_charsec("=")
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

    def _rule_name(self):
        return self._match_charsec("n")

    def _rule_choices(self):
        return self._match_charsec("c")

    def _rule_space(self):
        return self._match_charsec(" ")


import pprint
pprint.pprint(Parser().run("grammar", "n {n =c }"))
