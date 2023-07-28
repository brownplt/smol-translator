def make_counter(count):
    return lambda: 
    count = count + 1
    return count
    end
f = make_counter(0)
g = make_counter(0)
print(f())
print(f())
print(g())