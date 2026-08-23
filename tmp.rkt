#lang rhombus

def x = 1
fun f():
  x

fun g():
  def x = 2
  f()

g()
