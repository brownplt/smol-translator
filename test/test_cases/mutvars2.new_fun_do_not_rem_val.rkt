#lang rhombus

def mutable x = 1
fun f(n):
  x + n

x := 2
f(30)
