#lang rhombus

fun build_dbl():
  def mutable n = 1
  fun ():
    n := n * 2
    n

def dbl1 = build_dbl()
def dbl2 = build_dbl()

dbl1()
dbl2()
dbl1()
