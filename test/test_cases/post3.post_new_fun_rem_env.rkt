#lang rhombus

def mutable n = 7
fun foo():
  n

fun bar():
  n := 3
  foo()

bar()
n := 5
foo()
