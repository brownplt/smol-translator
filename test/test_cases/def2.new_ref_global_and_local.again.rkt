#lang rhombus

def a = 9
fun fun():
  def b = 2
  fun prod():
    a * b

  prod()

fun()
