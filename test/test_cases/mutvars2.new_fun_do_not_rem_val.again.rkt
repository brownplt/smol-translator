#lang rhombus

def mutable o = 1
fun u(t):
  o + t

o := 9
u(5)
