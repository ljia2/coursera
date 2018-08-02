array[int] of int: readings;
int: start;

set of int: TIME0 = 0 .. length(readings);
set of int: TIME = 1 .. length(readings);
array[TIME0] of var 25..30: temp;

constraint forall(t in TIME)(temp[t] = (temp[t-1] + readings[t]) div 2 + adjust[chioce[t]]);
constraint temp[0] = start;

enum ACTIONS = {heat, strongly_heat, cool, strongly_cool, do_nothing};
array[ACTIONS] of int: adjust = [1, 4, -2, -5, 0];
array[ACTIONS] of int: cost = [1, 5, 3, 9, 0];
array[TIME] of var ACTIONS: chioce;
var int: total_cost = sum(t in TIME)(cost[chioce[t]]);
solve minimize total_cost;

output ["total_cost = \(total_cost)\ntemp = \(temp)\nchioce=\(chioce)"]