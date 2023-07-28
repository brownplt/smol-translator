def make_getter():
    x = 1
    def get_x():
        return x
    return get_x
get_x = make_getter()
print(get_x())