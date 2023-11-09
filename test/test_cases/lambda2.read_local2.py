def make_getter(x):
    def get_x():
        return x
    return get_x
get_a = make_getter(1)
get_b = make_getter(2)
print(get_a())
print(get_b())