#lang rhombus

def mutable a = 1
fun foobar(b):
  a := 2
  b

foobar(a)
a
