#lang rhombus

def a = Array(55, 17)
fun foobar(b):
  b[0] := 52

foobar(a)
a
