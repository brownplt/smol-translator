#lang rhombus

def x = Array(10, 48, 95)
fun f(y):
  y[0] := 32
  y[0]

def z = f(x)
x[0]
