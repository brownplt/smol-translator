#lang rhombus

def mutable a = 2
fun make():
  fun (b):
    a + b

def fun = make()
a := 1
fun(1)
