def k(b):
    return lambda a: a + b
foo = k(3)
bar = k(2)
print(foo(3))
print(bar(3))