def get():
    return 42
f = get
g = f
print(g())