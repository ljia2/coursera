int: n; % number of court members
set of int: COURT = 1..n;
int: emperor = 1;
array[COURT] of int: rank;
array[COURT] of int: ability;

int: m; % number of horses
set of int: HORSE = 1..m;
array[HORSE] of int: beauty;
array[HORSE] of int: speed;

array[COURT,HORSE] of int: enjoy;

array[COURT] of var HORSE union {0}: court2horse;
array[HORSE] of var COURT union {0}: horse2court;

constraint court2horse[emperor] > 0;

constraint forall(c in COURT where c != emperor /\ court2horse[c] > 0)(enjoy[c, court2horse[c]] < enjoy[emperor, court2horse[emperor]]);

constraint forall(c in COURT, h in HORSE) ((court2horse[c] = h <-> horse2court[h] = c));

include "alldifferent_except_0.mzn";
constraint alldifferent_except_0(court2horse);
constraint sum(h in HORSE)(horse2court[h] > 0) = min(n, m);

constraint forall(c1, c2 in COURT where rank[c1] > rank[c2] ) 
   ((court2horse[c1] > 0 /\ court2horse[c2] > 0 /\ beauty[court2horse[c1]] >= beauty[court2horse[c2]]) 
     \/ court2horse[c2] = 0 
     \/ (court2horse[c2] = 0 /\ court2horse[c1] = 0));
    
var int: penality;

constraint penality = sum([100 | h1, h2 in HORSE where not (speed[h1] > speed[h2] -> ((horse2court[h2] > 0 /\ horse2court[h2] > 0 /\ ability[horse2court[h1]] >= ability[horse2court[h2]]) \/ horse2court[h1] = 0 \/ (horse2court[h1] = 0 /\ horse2court[h2] = 0)))]);                    
                                            
constraint forall(h in HORSE, c in COURT where court2horse[c] > 0 /\ horse2court[h] > 0)(enjoy[c, h] > 0); 
 
var int: obj;
constraint obj = sum(c in COURT where court2horse[c] > 0)(enjoy[c, court2horse[c]]) - penality;

solve maximize obj;

output ["horse = ["] ++ [show(court2horse[i]) ++ if i < n then "," else "];\n" endif | i in 1 .. n] ++ ["obj = \(obj)\n"];


