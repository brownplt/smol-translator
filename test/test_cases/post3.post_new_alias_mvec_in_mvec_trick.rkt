#lang rhombus

def mutable m = Array(66)
def z = Array(m, 66, 66)
m := Array(43)
z
