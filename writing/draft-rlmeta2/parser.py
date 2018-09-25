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
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("expr1")
                                    )())
                                ),
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq(":")
                                ),
                                (lambda:
                                    _vars.bind("y", (lambda:
                                        self._match("name")
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["Bind"]+[_vars.lookup("x").eval()]+[_vars.lookup("y").eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("expr1")
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_expr1(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("expr2")
                                    )())
                                ),
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("*")
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("expr2")
                                    )())
                                ),
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("?")
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("!")
                                ),
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("expr2")
                                    )())
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("expr2")
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_expr2(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("->")
                                ),
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("hostExpr")
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["SemanticAction"]+[_vars.lookup("x").eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
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
                                    self._negative_lookahead((lambda:
                                        self._or([
                                            (lambda:
                                                (lambda _vars:
                                                    (lambda:
                                                        self._and([
                                                            (lambda:
                                                                self._match("space")
                                                            ),
                                                            (lambda:
                                                                self._match_charseq("=")
                                                            ),
                                                        ])
                                                    )()
                                                )(_Vars())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["Apply"]+[_vars.lookup("x").eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("char")
                                    )())
                                ),
                                (lambda:
                                    self._match_charseq("-")
                                ),
                                (lambda:
                                    _vars.bind("y", (lambda:
                                        self._match("char")
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["MatchCharRange"]+[_vars.lookup("x").eval()]+[_vars.lookup("y").eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("string")
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["MatchString"]+[_vars.lookup("x").eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("charseq")
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["MatchCharseq"]+[_vars.lookup("x").eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq(".")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["MatchAny"]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("(")
                                ),
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("choices")
                                    )())
                                ),
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq(")")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _vars.lookup("x").eval())
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("[")
                                ),
                                (lambda:
                                    _vars.bind("xs", (lambda:
                                        self._star((lambda:
                                            self._match("expr")
                                        ))
                                    )())
                                ),
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("]")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["MatchList"]+(["And"]+_vars.lookup("xs").eval()+[])+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_hostExpr(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("string")
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["String"]+[_vars.lookup("x").eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("charseq")
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["String"]+[_vars.lookup("x").eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("[")
                                ),
                                (lambda:
                                    _vars.bind("xs", (lambda:
                                        self._star((lambda:
                                            self._match("hostExprListItem")
                                        ))
                                    )())
                                ),
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("]")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["List"]+_vars.lookup("xs").eval()+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("{")
                                ),
                                (lambda:
                                    _vars.bind("xs", (lambda:
                                        self._star((lambda:
                                            self._match("buildExpr")
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
                                    _SemanticAction(lambda: (["ListBuilder"]+_vars.lookup("xs").eval()+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("x")
                                    )())
                                ),
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("(")
                                ),
                                (lambda:
                                    _vars.bind("xs", (lambda:
                                        self._star((lambda:
                                            self._match("hostExpr")
                                        ))
                                    )())
                                ),
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq(")")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["FnCall"]+[_vars.lookup("x").eval()]+_vars.lookup("xs").eval()+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
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
                                    _SemanticAction(lambda: (["VarLookup"]+[_vars.lookup("x").eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_hostExprListItem(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("~")
                                ),
                                (lambda:
                                    _vars.bind("x", (lambda:
                                        self._match("hostExpr")
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["ListItemSplice"]+[_vars.lookup("x").eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("hostExpr")
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_buildExpr(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq(">")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["IndentBuilder"]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("space")
                                ),
                                (lambda:
                                    self._match_charseq("<")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (["DedentBuilder"]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match("hostExpr")
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_string(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_charseq("\"")
                                ),
                                (lambda:
                                    _vars.bind("xs", (lambda:
                                        self._or([
                                            (lambda:
                                                (lambda _vars:
                                                    (lambda:
                                                        self._and([
                                                            (lambda:
                                                                self._negative_lookahead((lambda:
                                                                    self._match_charseq("\"")
                                                                ))
                                                            ),
                                                            (lambda:
                                                                self._match("innerChar")
                                                            ),
                                                        ])
                                                    )()
                                                )(_Vars())
                                            ),
                                        ])
                                    )())
                                ),
                                (lambda:
                                    self._match_charseq("\"")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: join(
                                        _vars.lookup("xs").eval(),
                                    ))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_charseq(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_charseq("'")
                                ),
                                (lambda:
                                    _vars.bind("xs", (lambda:
                                        self._or([
                                            (lambda:
                                                (lambda _vars:
                                                    (lambda:
                                                        self._and([
                                                            (lambda:
                                                                self._negative_lookahead((lambda:
                                                                    self._match_charseq("'")
                                                                ))
                                                            ),
                                                            (lambda:
                                                                self._match("innerChar")
                                                            ),
                                                        ])
                                                    )()
                                                )(_Vars())
                                            ),
                                        ])
                                    )())
                                ),
                                (lambda:
                                    self._match_charseq("'")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: join(
                                        _vars.lookup("xs").eval(),
                                    ))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_name(self):
        return self._match_charseq("n")

    def _rule_char(self):
        return self._match_charseq("#")

    def _rule_string(self):
        return self._match_charseq("#")

    def _rule_charseq(self):
        return self._match_charseq("#")

    def _rule_space(self):
        return self._match_charseq(" ")


import pprint
pprint.pprint(Parser().run("grammar", "n {n = |n }"))
