let
  power = i: n: if i == 0 then 1 else (power (i - 1) n) * n;
in
power 3 2
