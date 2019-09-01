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
