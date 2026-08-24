#lang rhombus

def x = 12
fun f(mutable y):
  y := 0
  x

f(x)
