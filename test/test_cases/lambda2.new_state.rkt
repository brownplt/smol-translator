#lang rhombus

def mutable x = 1
fun f():
  fun addx(y):
    x + y

  addx

def g = f()
x := 2
g(0)
