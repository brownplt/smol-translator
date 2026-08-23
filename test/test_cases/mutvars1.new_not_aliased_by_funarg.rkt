#lang rhombus

def mutable x = 12
fun f(y):
  x := 0
  y

f(x)
