% Beauty Trap
% Create the most magnificient feast 
enum DISH;
enum TASTE = {spicy, sour, salty, sweet, umame, bland};
enum TEMP = {hot, cold, warm};
%int: ndish;
%set of int: DISH = 1..ndish;
%set of int: TASTE = 1..6;
%int: spicy = 1; int: sour = 2; int: salty = 3; int: sweet = 4; int: umame = 5; int: bland = 6;
%set of int: TEMP = 1..3;
%int: hot = 1; int: cold = 2; int: warm = 3;


array[DISH] of TASTE: taste;
array[DISH] of TEMP: temp;
array[DISH] of bool: heavy;
array[DISH] of int: value;

int: len; % length of banquet
set of int: COURSE = 1..len;

array[COURSE] of var DISH: dish;

include "alldifferent.mzn";
constraint alldifferent(dish);
% Solution 1

%constraint taste[dish[1]] = salty;
%constraint taste[dish[len]] = sweet;
%constraint forall(i in 1 .. len-1)(taste[dish[i]] != taste[dish[i+1]]);
%constraint forall(i in 1 .. len-1)(taste[dish[i]] = spicy -> taste[dish[i+1]] in {bland, sweet});
%constraint forall(i in 1 .. len-1)(taste[dish[i]] = sour  -> taste[dish[i+1]] in {bland, umame});
%constraint forall(i in 1 .. len-1)(taste[dish[i]] = sweet -> taste[dish[i+1]] in {bland, salty, sour});

%constraint forall(i in 1 .. len-1)
%                  ((temp[dish[i]] = hot -> 
%                    forall(j in i+1 .. len)
%                    (temp[dish[j]] = cold -> exists(k in i+1 .. j-1)(temp[dish[k]] = warm))));

%constraint forall(i in 1 .. len-2)
%              (heavy[dish[i]] /\ heavy[dish[i+1]] -> not heavy[dish[i+2]]);

%var int: obj;
%constraint obj = sum(i in 1 .. len)(value[dish[i]])
%               + sum(i in 1 .. len-1)(taste[dish[i]] != taste[dish[i+1]])
%               + sum(i in 1 .. len-1)(temp[dish[i]] != temp[dish[i+1]])
%               + sum(i in 1 .. len-1)(heavy[dish[i]] != heavy[dish[i+1]]);
            
%solve maximize obj;

% replace constraints for tastes
include "regular.mzn";
constraint regular)([TASTE[dish[i]] | i in COURSE], 7, 6, d, 1, {5});
array[1..7, TASTE] of 0 .. 7: d = [|0, 0, 4, 0, 0, 0
                                   |0, 0, 0, 5, 0, 7
                                   |0, 0, 0, 0, 6, 7
                                   |2, 3, 0, 5, 6, 7
                                   |0, 3, 4, 0, 0, 7
                                   |2, 3, 4, 5, 0, 7
                                   |2, 3, 4, 5, 6, 0 |];
                                   
%constraint taste[dish[1]] = salty;
%constraint taste[dish[len]] = sweet;
%constraint forall(i in 1 .. len-1)(taste[dish[i]] != taste[dish[i+1]]);
%constraint forall(i in 1 .. len-1)(taste[dish[i]] = spicy -> taste[dish[i+1]] in {bland, sweet});
%constraint forall(i in 1 .. len-1)(taste[dish[i]] = sour  -> taste[dish[i+1]] in {bland, umame});
%constraint forall(i in 1 .. len-1)(taste[dish[i]] = sweet -> taste[dish[i+1]] in {bland, salty, sour});

constraint regular([temp[dish[i]] | i in COURSE], 2, 3, td, 1, {1, 2});
array[1..2, temp] of 0 .. 2: td = [|2, 1, 1
                                   |1, 0, 1 |];
                                                                    
%constraint forall(i in 1 .. len-1)
%                  ((temp[dish[i]] = hot -> 
%                    forall(j in i+1 .. len)
%                    (temp[dish[j]] = cold -> exists(k in i+1 .. j-1)(temp[dish[k]] = warm))));

constraint forall(i in 1 .. len-2)
              (heavy[dish[i]] /\ heavy[dish[i+1]] -> not heavy[dish[i+2]]);

var int: obj;
constraint obj = sum(i in 1 .. len)(value[dish[i]])
               + sum(i in 1 .. len-1)(taste[dish[i]] != taste[dish[i+1]])
               + sum(i in 1 .. len-1)(temp[dish[i]] != temp[dish[i+1]])
               + sum(i in 1 .. len-1)(heavy[dish[i]] != heavy[dish[i+1]]);
            
solve maximize obj;



