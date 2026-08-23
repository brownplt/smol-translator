#lang rhombus

fun bar(y):
  fun (x):
    x + y

def f = bar(2)
def g = bar(4)
f(2)
g(2)
