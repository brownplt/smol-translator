x = 1
def main():
    def get_x():
        return x
    x = 2
    return get_x()
print(main())