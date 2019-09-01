def leftAssoc(expr, items):
    while items:
        op, rhs = items.pop(0)
        expr = op(expr, rhs)
    return expr
def rightAssoc(expr, items):
    if items:
        op, rhs = items.pop(0)
        expr = op(expr, rightAssoc(rhs, items))
    return expr
class Op(object):

    def __init__(self, fn, prec, assoc):
        self.fn = fn
        self.prec = int(prec)
        self.assoc = assoc
def parseOps(expr, items, min_level=1):
    while items and items[0][0].prec >= min_level:
        op, rhs = items.pop(0)
        if op.assoc == "left":
            next_level = min_level + 1
        else:
            next_level = min_level
        expr = op.fn(expr, parseOps(rhs, items, next_level))
    return expr
