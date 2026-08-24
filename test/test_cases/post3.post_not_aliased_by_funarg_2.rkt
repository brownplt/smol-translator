#lang rhombus

def a = 5
fun k(mutable b):
  b := 3
  a

k(a)
