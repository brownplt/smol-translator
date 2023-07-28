def make_counter(count):
    return lambda: [count := count + 1, count][-1]
f = make_counter(0)
g = make_counter(0)
print(f())
print(f())
print(g())