#lang rhombus

fun foo():
  def mutable n = 0
  fun bar():
    n := n + 1
    n

  bar

def f = foo()
def g = foo()

f()
f()
g()
