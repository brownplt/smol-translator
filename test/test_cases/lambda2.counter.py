def make_counter(count):
    def counter():
        count := count + 1
        return count
    return counter
f = make_counter(0)
g = make_counter(0)
print(f())
print(f())
print(g())