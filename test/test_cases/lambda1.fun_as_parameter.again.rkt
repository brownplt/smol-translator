#lang rhombus

fun ffx(f, x):
  f(f(x))

fun inc(x):
  x + 1

ffx(inc, 1)
