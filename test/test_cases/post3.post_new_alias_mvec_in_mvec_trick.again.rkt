#lang rhombus

def mutable x = Array(0)
def v = Array(2, x, 3)
x := Array(1)
v
