#lang rhombus

def mutable x = 1
fun f():
  fun (y):
    x + y

def g = f()
x := 2
g(0)
