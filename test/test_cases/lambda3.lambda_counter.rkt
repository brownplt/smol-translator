#lang rhombus

fun foobar():
  def mutable n = 0
  fun ():
    n := n + 1
    n

def f = foobar()
def g = foobar()

f()
f()
g()
