#lang rhombus

def n = 5
fun f1(m):
  fun f2():
    def l = 4
    n + m + l

  f2()

f1(1) + 3
