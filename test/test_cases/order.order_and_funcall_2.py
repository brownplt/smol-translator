x = 2
def main():
    def getx():
        return x
    y = getx()
    x = 3
    return y
print(main())