#lang rhombus

def mutable x = 12
fun f():
  x

fun g():
  x := 0
  f()

g()
x := 1
f()
