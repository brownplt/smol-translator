def mutable mv = Array(63)
def mutable mv2 = Array(mv, mv)
mv2[0][0] := 42
mv2[1]