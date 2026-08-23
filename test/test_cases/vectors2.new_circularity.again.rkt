#lang rhombus

def a = Array(41, 92)
def b = Array(a)
a[1] := b
a[0]
