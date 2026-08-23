#lang rhombus

def a = 3
fun foo(b):
  fun bar():
    def c = 6
    a + b + c

  bar()

foo(4) + 2
