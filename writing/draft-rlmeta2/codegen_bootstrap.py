class CodeGenerator(_RLMeta):
    def ast(self):
        _ors = []
        def _label1():
            ...
        _ors.append(_label1)
        return self._or(_ors)
