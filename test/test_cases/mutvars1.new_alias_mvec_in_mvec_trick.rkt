#lang rhombus

def mutable x = Array(55)
def v = Array(x, 55, 55)
x := Array(66)
v
