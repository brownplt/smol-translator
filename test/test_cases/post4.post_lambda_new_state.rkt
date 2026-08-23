#lang rhombus

def mutable a = 1
fun foo():
  fun (b):
    a + b

def bar = foo()
a := 3
bar(0)
