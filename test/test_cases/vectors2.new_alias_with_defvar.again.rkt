#lang rhombus

def foo = Array(65, 48)
def bar = foo
bar[0] := 55
foo
