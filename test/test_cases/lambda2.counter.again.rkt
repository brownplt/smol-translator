#lang rhombus

fun f():
  def mutable n = 1
  fun dbl():
    n := n * 2
    n

  dbl

def dbl1 = f()
def dbl2 = f()

dbl1()
dbl2()
dbl1()
