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


object(placed_soup, X, Y) :-
    placed_soup(X, Y) &
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, X, Y, D) &
    not (placed_soup(X1, Y1) &
         manhattan_distance(AX, AY, X1, Y1, D1) &
         D1 < D).

object(placed_onion, X, Y) :-
    placed_onion(X, Y) &
    can_reach_agent(counter,X,Y) &
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, X, Y, D) &
    not (placed_onion(X1, Y1) &
         manhattan_distance(AX, AY, X1, Y1, D1) &
         D1 < D).

object(placed_tomato, X, Y) :-
    placed_tomato(X, Y) &
    can_reach_agent(counter,X,Y) &
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, X, Y, D) &
    not (placed_tomato(X1, Y1) &
         manhattan_distance(AX, AY, X1, Y1, D1) &
         D1 < D).

object(placed_dish, X, Y) :-
    placed_dish(X, Y) &
    can_reach_agent(counter,X,Y) &
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, X, Y, D) &
    not (placed_dish(X1, Y1) &
         manhattan_distance(AX, AY, X1, Y1, D1) &
         D1 < D).

object(active_pot, X,Y) :-
    active_pot(X,Y).

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

placed_item(placed_onion, X, Y) :- placed_onion(X, Y).
placed_item(placed_tomato, X, Y) :- placed_tomato(X, Y).
placed_item(placed_soup, X, Y) :- placed_soup(X, Y).
placed_item(placed_dish, X, Y) :- placed_dish(X, Y).

placedObject(Thing,X,Y) :-
    placed_item(Thing,X,Y) &
    can_reach_agent(counter,X,Y).

!start.


+!start : begin(now)
<- 

    ?recipes(Recipes);
    .print("Retrieved recipes: " , Recipes);
    ?count_objects(pot, Count);
    .wait(300);
    !can_reach_ingredient_plate;
    !can_reach_ingredient_serve;
    !can_reach_ingredient;
    !can_reach_plate;
    !can_reach_serve;
    !can_reach_pot.
 
 +!start : not begin(now)
<-  
    !start.

///// CASE ONLY CAN REACH INGREDIENT////
+!can_reach_ingredient : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & recipes([])
<-
    .print("All recipes completed").

+!can_reach_ingredient : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, NOnion);
    ?countInRecipe(tomato, FirstRecipe, NTomato);
    !pass_to_human(onion,NOnion);
    !pass_to_human(tomato,NTomato);
    .wait(3000);
    !can_reach_ingredient.

+!can_reach_ingredient
<-
    .print("Plan 'only ingredient' is not applicable here").
////////////

///// CASE ONLY CAN REACH PLATE////
+!can_reach_plate : can_reach_agent(plate,X,Y) & recipes([])
<-
    .print("All recipes completed").

+!can_reach_plate : can_reach_agent(plate,X,Y) & recipes([FirstRecipe|_])
<-
    !pass_to_human(plate,1);
    !wait_for_soup;
    .wait(3000);
    !can_reach_plate.

+!can_reach_plate
<-
    .print("Plan 'only plate' is not applicable here").
////////////

///// CASE ONLY CAN REACH SERVE////
+!can_reach_serve : can_reach_agent(serve,X,Y) & recipes([])
<-
    .print("All recipes completed").

+!can_reach_serve : can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_])
<-
    !pick_from_human(placed_soup,1);
    .print("do no print");
    !go_to(serve);
    !can_reach_serve.

+!can_reach_serve
<-
    .print("Plan 'only serve' is not applicable here").
////////////

///// CASE ONLY CAN REACH POT////
+!can_reach_pot : can_reach_agent(pot,X,Y) & recipes([])
<-
    .print("All recipes completed").

+!can_reach_pot : can_reach_agent(pot,X,Y) & recipes([FirstRecipe|_]) & active_pot(X,Y)
<-
    ?countInRecipe(onion, FirstRecipe, NOnion);
    ?countInRecipe(tomato, FirstRecipe, NTomato);
    !pick_from_human_pot(placed_onion,NOnion);
    !pick_from_human_pot(placed_tomato,NTomato);
    !pick_from_human(placed_dish,1);
    !go_to(active_pot);
    !wait_for_soup;
    !execute;
    !go_to(counterTo);
    remove_beliefs(active_pot(X,Y));
    !wait_to_serve;
    !can_reach_pot.

+!can_reach_pot : can_reach_agent(pot,X,Y) & recipes([FirstRecipe|_]) & not(active_pot(X,Y))
<-
    !choose_pot_for_recipe(FirstRecipe);
    !can_reach_pot.

+!can_reach_pot
<-
    .print("Plan 'only pot/s' is not applicable here").
    /////////////

+!wait_to_serve
<-
    .wait(5000).

// CASE CAN ONLY REACH INGREDIENT + PLATE /////////
+!can_reach_ingredient_plate : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & recipes([])
<-
    .print("All recipes completed").

+!can_reach_ingredient_plate : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, NOnion);
    ?countInRecipe(tomato, FirstRecipe, NTomato);
    !pass_to_human(onion,NOnion);
    !pass_to_human(tomato,NTomato);
    .wait(400);
    !pass_to_human(plate,1);
    .wait(3000);
    !can_reach_ingredient_plate.

+!can_reach_ingredient_plate
<- 
    .print("this plan is not applicable").
/////////////////////////

// CASE CAN ONLY REACH INGREDIENT + SERVE /////////
+!can_reach_ingredient_serve : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y) & recipes([])
<-
    .print("All recipes completed").

+!can_reach_ingredient_serve : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, NOnion);
    ?countInRecipe(tomato, FirstRecipe, NTomato);
    !pass_to_human(onion,NOnion);
    !pass_to_human(tomato,NTomato);
    .wait(400);
    !wait_for_soup;
    !pick_from_human(placed_soup,1);
    !go_to(serve);
    .wait(3000);
    !can_reach_ingredient_serve.

+!can_reach_ingredient_serve
<- 
    .print("this plan is not applicable").
/////////////////////////


// PICKING STUFF UP FROM HUMAN
+!pick_from_human(Thing,0)
<-
    .print("All things picked up from the counter").

+!pick_from_human(Thing,N) :  placedObject(Thing,X,Y) & N>0
<-
    !go_to(Thing);
    Nnew = N - 1;
    !pick_from_human(Thing,Nnew).

+!pick_from_human(Thing,N)
<-
    .print("waiting for object to be passed from human");
    .wait(2000);
    .print(Thing);
    !pick_from_human(Thing,N).
/////////////
// PICKING STUFF UP FROM HUMAN
+!pick_from_human_pot(Thing,0)
<-
    !execute;
    .print("All things picked up from the counter").

+!pick_from_human_pot(Thing,N) :  placedObject(Thing,X,Y) & N>0
<-
    !go_to(Thing);
    !go_to(active_pot);
    Nnew = N - 1;
    !pick_from_human_pot(Thing,Nnew).

+!pick_from_human_pot(Thing,N)
<-
    .print("waiting for object to be passed from human");
    .wait(500);
    .print(Thing);
    !pick_from_human_pot(Thing,N).
/////////////
// PASSING STUFF WHEN THERE IS A TABLE/STH ELSE IN BETWEEN
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
    .print("no free counters to pass things");
    !pass_to_human(Ingredient,N).
//////////////////////////////////////////
///////////// Choose a pot for a recipe
+!choose_pot_for_recipe(Recipe)
: pot(X,Y) &
  not (pot_contents(X,Y, onion, OCount)) &
  not (pot_contents(X,Y, tomato, TCount)) 
<-
  .print("Pot (", Z, ",", M, ") is empty => using it for recipe: ", Recipe);
  add_beliefs(active_pot(X,Y)).


+!choose_pot_for_recipe(Recipe)
: pot(X,Y) &
  not pot_matches_recipe(Z,M,Recipe)
<-
  ?pot_contents(X,Y, onion, OCount);
  ?pot_contents(X,Y, tomato, TCount);
  .print(OCount, TCount);
  .print("Pot (", Z, ",", M, ") has wrong ingredients => discarding them now...");
  !discard_soup(Z,M);
  !choose_pot_for_recipe(Recipe).


+!choose_pot_for_recipe(Recipe) : pot(X,Y) & pot_matches_recipe(X,Y,Recipe)
<-
  .print("Pot (", X, ",", Y, ") partially matches a recipe - use that one");
  add_beliefs(active_pot(X,Y)).

+!discard_soup
<-
    !go_to(pot);
    !go_to(plate);
    !go_to(pot);
    !go_to(serve).
    
//+!wait_for_soup : pot_ready(X,Y)
//<-
  //  .print("soup is ready to be picked up").


+!wait_for_soup
<-
    .print("waiting for soup to cook");
    .wait(5000).
    //!wait_for_soup.



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
////////////////////////////////////////

////////////// Turn to object ///////////////
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
////////////////////////////////////