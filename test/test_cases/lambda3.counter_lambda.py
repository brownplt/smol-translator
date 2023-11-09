def make_counter(count):
    return lambda: ("WARNING: the translation might be inaccurate", (count := count + 1, count)[-1])[-1]
f = make_counter(0)
g = make_counter(0)
print(f())
print(f())
print(g())