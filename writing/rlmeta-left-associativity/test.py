def parse():
    return (1, [
        ['-', 2],
        ['-', 3],
    ])

def left_assoc(expr, items):
    while items:
        op, rhs = items.pop(0)
        expr = [op, expr, rhs]
    return expr

def right_assoc(expr, items):
    if items:
        op, rhs = items.pop(0)
        expr = [op, expr, right_assoc(rhs, items)]
    return expr

def eval_expr(expr):
    if isinstance(expr, list):
        op, lhs, rhs = expr
        if op == "-":
            return eval_expr(lhs) - eval_expr(rhs)
        else:
            raise Exception("unknown operator {!r}".format(op))
    else:
        return expr

def echo_eval_expr(expr):
    print("{} => {}".format(expr, eval_expr(expr)))

echo_eval_expr(left_assoc(*parse()))
echo_eval_expr(right_assoc(*parse()))
