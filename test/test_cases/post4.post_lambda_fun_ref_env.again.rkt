#lang rhombus

fun f(n):
  fun (m):
    m * n

def fun1 = f(10)
def fun2 = f(1)
fun1(4)
fun2(4)
