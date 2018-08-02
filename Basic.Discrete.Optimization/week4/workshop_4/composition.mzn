int: n;

set of int: NOTE = 1 .. n;
set of int: ORDER = 1 .. n;

set of int: NOTE1 = 1 .. n-1;
set of int: ORDER1 = 1 .. n-1;

array[ORDER] of var NOTE: order;
array[ORDER1] of var NOTE1: diffs;

constraint forall(i in 1 .. n - 1)(diffs[i] = abs(order[i] - order[i+1]));

include "globals.mzn";
constraint alldifferent(order);
constraint alldifferent(diffs);

solve satisfy;

int: d = ceil(ln(n+1) / ln(10));
string: spaces = concat([" "| i in 1 .. d]);
% show_int(d, i) will pad i with white spaces to d digits. 
output([show_int(d, order[i]) ++ spaces | i in 1 .. n] ++ ["\n", spaces] ++ [show_int(d, diffs[i]) ++ spaces | i in 1 .. n-1]);
