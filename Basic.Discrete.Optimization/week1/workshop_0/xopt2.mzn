var 0 .. 10: x;
var int: y;
constraint y * 4 = x;
var float: obj = (x - 7) * (x - 7);
solve minimize obj;
output["x = \(x) \nobj = \(obj)"];