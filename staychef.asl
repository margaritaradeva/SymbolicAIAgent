// Helper function calculating manhattan distance
delta(X, Y, D) :- X >= Y & D = X - Y.
delta(X, Y, D) :- X < Y & D = Y - X.
manhattan_distance(X, Y, Z, M, D) :- delta(X, Z, D1) & delta(Y, M, D2) & D = D1 + D2.

// check if a tile is passable - there is no human on it or countertoops, pots etc
free_tile(Z, M) :-  terrain(Z,M,Term) & .term2string(Term, StringTerm) & StringTerm == "e".
free_of_human(X,Y) :- not human_position(X,Y).

countInRecipe(_, [], 0).

countInRecipe(I, [I|T], N) :-
    countInRecipe(I, T, N1) &
    N = N1 + 1.

countInRecipe(I, [X|T], N) :-
    not (X=I) &
    countInRecipe(I, T, N).

pot_matches_recipe(Z,M,Recipe) :-
    pot_contents(Z,M, onion, OCount) &
    pot_contents(Z,M, tomato, TCount) &
    countInRecipe(onion,  Recipe, ROnionCount) &
    countInRecipe(tomato, Recipe, RTomatoCount) &
    ((OCount  < ROnionCount) | (OCount  = ROnionCount)) &
    ((TCount  < RTomatoCount) | (TCount  = RTomatoCount)).

// check for adjacency - e.g. are we next to the pot?
adjacent(X, Y, Z, M) :- (X = Z & ((Y=M+1) | (Y=M-1))) | (Y=M & ((X = Z + 1) | (X = Z - 1))).

// count how many objects we have of a certain type - eg how many pots
// need to pass it as ?count_objects(pot(X,Y), N)
count_objects(Object, Count) :- .count(Object, Count).


object(pot, Z, M) :-
    pot(Z, M) &
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, Z, M, D) &
    not (pot(Z1, M1) &
         manhattan_distance(AX, AY, Z1, M1, D1) &
         D1 < D).

object(plate, Z, M) :-
    plate(Z, M) &
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, Z, M, D) &
    not (plate(Z1, M1) &
         manhattan_distance(AX, AY, Z1, M1, D1) &
         D1 < D).

object(serve, Z, M) :-
    serve(Z, M) &
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, Z, M, D) &
    not (serve(Z1, M1) &
         manhattan_distance(AX, AY, Z1, M1, D1) &
         D1 < D).

object(tomato, Z, M) :-
    tomato(Z, M) &
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, Z, M, D) &
    not (tomato(Z1, M1) &
         manhattan_distance(AX, AY, Z1, M1, D1) &
         D1 < D).

object(onion, Z, M) :-
    onion(Z, M) &
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, Z, M, D) &
    not (onion(Z1, M1) &
         manhattan_distance(AX, AY, Z1, M1, D1) &
         D1 < D).




possible_counter(Z,M) :-
    can_reach_agent(counter,Z,M) &
    can_reach_human(counter,Z,M) &
    not placed_stuff(Z,M).

closest_counter(Z,M) :-
    possible_counter(Z,M) &
    human_position(Hx, Hy) &
    manhattan_distance(Hx, Hy, Z, M, D) &
    not ( possible_counter(Z1,M1)
        & manhattan_distance(Hx, Hy, Z1, M1, D1)
        & D1 < D ).

object(counterTo, Z, M) :-
    closest_counter(Z, M).

!start.


+!start : begin(now)
<- 

    ?recipes(Recipes);
    .print("Retrieved recipes: " , Recipes);
    .wait(300);
    !can_reach_onion_plate.
 
 +!start : not begin(now)
<-  
    !start.

+!can_reach_onion_plate : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & recipes([])
<-
    .print("All recipes completed").

+!can_reach_onion_plate : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, NOnion);
    ?countInRecipe(tomato, FirstRecipe, NTomato);
    !pass_to_human(onion,NOnion);
    !pass_to_human(tomato,NTomato);
    .wait(400);
    !pass_to_human(plate,1);
    .wait(5000);
    !can_reach_onion_plate.

+!can_reach_onion_plate
<- 
    .print("this plan is not applicable").

+!pass_to_human(Ingredient,0)
<-
    .print("All things passed successfully").

+!pass_to_human(Ingredient,N) : can_reach_agent(Ingredient,Z,M) & object(counterTo,X,Y) & N>0
<-
    !go_to(Ingredient);
    !go_to(counterTo);
    Nnew = N-1;
    !pass_to_human(Ingredient,Nnew).

+!pass_to_human(Ingredient,N) : N>0
<-  
    .wait(500);
    .print("no free counters to pass onions");
    !pass_to_human(Ingredient,N).







+!discard_soup
<-
    !go_to(pot);
    !go_to(plate);
    !go_to(pot);
    !go_to(serve).
    

//////////// Go to  ////////////////
+!go_to(Object) :  agent_position(X,Y) & object(Object,Z,M) & adjacent(X,Y,Z,M)
<- 
    .print("arrived");
    .wait(700);
    !turn_to(Object).

+!go_to(Object) : agent_position(X,Y) & object(Object,Z,M) & not adjacent(X,Y,Z,M) 
<-
    .print("Going to", object(Object, Z,M));
    compute_path(Z,M);
    !go_to(Object).


////////////// Turn to object 
+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (X < Z)
<- 
    action("right");
    !execute.

+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (Y > M) 
<- 
    action("up");
    !execute.

+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (Y < M)
<- 
    action("down");
    !execute.

+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (X > Z)
<- 
    action("left");
    !execute.

+!execute : true
<-
    action("space").