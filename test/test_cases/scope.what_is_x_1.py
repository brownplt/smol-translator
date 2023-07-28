x = 1
def main():
    x = 2
    def get_x():
        return x
    return get_x()
print(main())