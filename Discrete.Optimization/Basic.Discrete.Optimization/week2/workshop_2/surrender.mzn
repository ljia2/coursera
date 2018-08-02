% workshop 2
% select a set of trade negotiation party
enum NEGOTIATOR;
NEGOTIATOR: dummy; 

int: l; % minimum party size
int: u; % maximum party size
int: m; % minimum joint ability

array[NEGOTIATOR] of int: honor;
array[NEGOTIATOR,NEGOTIATOR] of int: joint;

array[1 .. u] of var NEGOTIATOR: party;
constraint forall(i in 1 .. u-1)(party[i] >= (party[i] != dummy) + party[i+1]);
constraint sum(i in 1 .. u)(party[i] != dummy) >= l;
constraint sum(i, j in 1 .. u where i < j)(joint[party[i], party[j]]) >= m;
var int: minHonor;
% penality on dummy's honor to avoid minHonor to take dummy's honor
constraint minHonor = min(i in 1 .. u)(honor[party[i]] + (party[i] = dummy) * (max(honor) + 1));

solve maximize minHonor;
