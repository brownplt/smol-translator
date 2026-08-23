#lang rhombus

def cat = 7
fun k(dog):
  def cat = 4
  cat + dog

k(1) + cat
