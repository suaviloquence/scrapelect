import random

def csscrape():
    return [[statement_list]]

def statement_list():
    return [[statement, statement_list]]

def statement():
    return [[randid(), ":", rvalue, filter_list, ";"]]

def rvalue():
    return [[element], [leaf]]

def element():
    return [[maybe_url, selector_list, selector_ops, "{", statement_list, "}"]]

def maybe_url():
    return [[variable], [randstr()]]

def selector_list():
    return [[selector, selector2]]

def selector():
    return [[".", randid()], ["#", randid()], [randid()], ["*"]]

def selector2():
    return [[selector_combinator, selector, selector2]]

def selector_combinator():
    # explicit whitespace here
    return [[x] for x in [" + ", " > ", " ~ ", ""]]

def selector_ops():
    return [["?"], ["[]"]]

def filter_list():
    return [["|", randid(), "(", arg_list, ")", filter_list]]

def arg_list():
    return [[randid(), ":", leaf, arg_list2]]

def arg_list2():
    return [[",", arg_list]]

def leaf():
    return [[variable], [randflt()], [randstr()], [randint()]]

def variable():
    return [["$", randid()]]

e = lambda: ""
def randid():
    return "".join(random.choice("abcdefghjiklmnopqrstuvwxyz") for _ in range(random.randint(1, 26)))

def randflt():
    return str((1 - random.random()) * 10000)

def randint():
    return str(random.randint(-10000, 10000))

def randstr():
    # doesn't test escaping or anything
    return f'"{randid()}"'

bases = {
    statement_list,
    maybe_url,
    selector2,
    selector_combinator,
    selector_ops,
    filter_list,
    arg_list,
    arg_list2
}

def fuzz(recurse_prob, f=csscrape, a=None):
    if a is None: a = recurse_prob
    if f in bases:
        r = random.random()
        if r < 1 - recurse_prob:
            yield ""
            return

    rule = random.choice(f())
    for r in rule:
        if isinstance(r, str):
            yield r
        else:
            yield "".join(fuzz(recurse_prob * a, f=r))

if __name__ == '__main__':
    import sys
    r = float(sys.argv[1])
    for x in fuzz(r): print(x)
