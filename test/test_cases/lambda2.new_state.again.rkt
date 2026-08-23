#lang rhombus

def mutable a = 2
fun make():
  fun f(b):
    a + b

  f

def g = make()
a := 1
g(1)
