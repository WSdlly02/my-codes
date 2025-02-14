let
  f = i: n: if (i <= 2) then f (i + 1) (n + 2) else n;
in
f 1 2
