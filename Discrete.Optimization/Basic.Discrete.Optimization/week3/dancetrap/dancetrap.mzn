% Beauty Trap
% Perform the most seductive dance

enum LEGS = {spin, leap, waltz, curtsey, prep, stand};
enum ARMS = {beckon, out, up, wrapped, neutral};
enum FACE = {smile, wink, batt, think, glow, blank};

int: maxlen;
set of int: STEP = 1..maxlen;
array[LEGS] of int: maxlegs;
array[ARMS] of int: maxarms;
array[FACE] of int: maxface;
constraint assert(maxlegs[stand] >= maxlen, "maxlegs[stand] smaller than maxlen");
constraint assert(maxarms[neutral] >= maxlen, "maxarms[neutral] smaller than maxlen");
constraint assert(maxface[blank] >= maxlen, "maxface[blank] smaller than maxlen");
array[LEGS,ARMS] of int: dance_value;
array[ARMS,FACE] of int: entice_value;
constraint assert(dance_value[stand,neutral] = 0, "incorrect dance_value array");
constraint assert(entice_value[neutral,blank] = 0, "incorrect entice_value array");
int: boredom; % how bored each step make the viewer

var STEP: len; % variable of dance length
array[STEP] of var LEGS: legs;
array[STEP] of var ARMS: arms;
array[STEP] of var FACE: face;

include "globals.mzn";

% must leap before prep
constraint forall(i in 2 .. maxlen)(legs[i] = leap -> legs[i-1] = prep);
% only curtsey, prep and stand directly after spin
constraint forall(i in 1 .. maxlen-1)(legs[i] = spin -> legs[i+1] in {curtsey, prep, stand}); 
% only spin, waltz and stand directly after leap             
constraint forall(i in 1 .. maxlen-1)(legs[i] = leap -> legs[i+1] in {spin, waltz, stand});
% can not prep directly after curtsey 
constraint forall(i in 1 .. maxlen-1)(legs[i] = curtsey -> not(legs[i+1] = prep));
% there is a stand between a waltz and a following curtsey
constraint forall(i, j in 1 .. maxlen where i < j) ((legs[i] = waltz /\ legs[j] = curtsey) -> exists(k in i+1 .. j-1)(legs[k] = stand));
% at most three waltz in a row
constraint forall(i in 1 .. maxlen-3)(legs[i] = legs[i+1] /\ legs[i+1] = legs[i+2] /\ legs[i+2] = waltz -> not(legs[i+3] = waltz));
% can not exceed max LEGS movements
constraint global_cardinality_low_up(legs, LEGS, [0|i in LEGS], maxlegs);                                                               
                      
% beckon must directly after out and up                                                                                                                                                                                                  %                                                                                                                                                                              
constraint not (arms[1] = beckon);
constraint forall(i in 2 .. maxlen)(arms[i] = beckon -> arms[i-1] in {out, up});
% wrapped must directly after neutral or beckon
constraint forall(i in 2 .. maxlen)(arms[i] = wrapped -> arms[i-1] in {neutral, beckon});
% cannot two same arm movements in a row except neutral
constraint forall(i in 1 .. maxlen-1)(arms[i] = arms[i+1] -> arms[i] = neutral);

% can not exceed max ARMS movements
constraint global_cardinality_low_up(arms, ARMS, [0|i in ARMS], maxarms); 
  
% can not make more than two facial gestures in a row execpet blank                                                                                             
constraint forall(i in 1 .. maxlen-2)((face[i] = face[i+1] /\ face[i+1] = face[i+2]) -> face[i] = blank);   
% can not think directly after smile or batt                                                                                                                 
constraint forall(i in 1 .. maxlen-1)(face[i] in {smile, batt} -> not (face[i+1] = think));              
% can not exceed max FACE gestiures
constraint global_cardinality_low_up(face, FACE, [0|i in FACE], maxface); 
%avoid negative-valued combinations of leg and arm
constraint forall(i in 1 .. maxlen)(dance_value[legs[i], arms[i]] >= 0);
%avoid negiative-valued combinations of arm and face
constraint forall(i in 1 .. maxlen)(entice_value[arms[i], face[i]] >= 0);
%must stay stand, meutral and blank after dance.
constraint forall(i in len +1 .. maxlen)(legs[i] = stand /\ arms[i] = neutral /\ face[i] = blank);

var int: obj;

constraint obj = sum(i in 1 .. len)(dance_value[legs[i], arms[i]] + entice_value[arms[i], face[i]]) - boredom * len;

solve maximize obj;                            
                
                
                