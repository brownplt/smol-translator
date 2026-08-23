#lang rhombus

def mutable a = Array(88)
def c = Array(a, 88, 88)
a := Array(76)
c
