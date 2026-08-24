#lang rhombus

def s = 1
fun foobar(mutable t):
  t := 2
  s

foobar(s)
