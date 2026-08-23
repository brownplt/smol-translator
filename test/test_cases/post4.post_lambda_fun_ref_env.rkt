#lang rhombus

fun k(b):
  fun (a):
    a + b

def foo = k(3)
def bar = k(2)
foo(3)
bar(3)
