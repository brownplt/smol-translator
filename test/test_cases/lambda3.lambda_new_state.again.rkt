#lang rhombus

def mutable a = 2
fun make():
  fun (b):
    a + b

def fun_ = make()
a := 1
fun_(1)
