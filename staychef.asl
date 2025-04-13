// Helper function calculating manhattan distance
delta(X, Y, D) :- X >= Y & D = X - Y.
delta(X, Y, D) :- X < Y & D = Y - X.
manhattan_distance(X, Y, Z, M, D) :- delta(X, Z, D1) & delta(Y, M, D2) & D = D1 + D2.


route_cost(Object, D) :-
 agent_position(X,Y) &
  object(Object, Z, M) &
   manhattan_distance(X,Y,Z,M,D).

route_cost_human(Object, D) :-
human_position(X,Y) &
  object(Object, Z, M) &
   manhattan_distance(X,Y,Z,M,D).


countInRecipe(_, [], 0).

countInRecipe(I, [I|T], N) :-
    countInRecipe(I, T, N1) &
    N = N1 + 1.

countInRecipe(I, [X|T], N) :-
    not (X=I) &
    countInRecipe(I, T, N).



// check for adjacency - e.g. are we next to the pot?
adjacent(X, Y, Z, M) :- (X = Z & ((Y=M+1) | (Y=M-1))) | (Y=M & ((X = Z + 1) | (X = Z - 1))).

// count how many objects we have of a certain type - eg how many pots
// need to pass it as ?count_objects(pot(X,Y), N)
count_objects(Object, Count) :- .count(Object, Count).


object(pot, Z, M) :-
    pot(Z, M) &
    can_reach_agent(pot,Z,M) & 
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, Z, M, D) &
    not (pot(Z1, M1) &
         manhattan_distance(AX, AY, Z1, M1, D1) &
         D1 < D).

object(plate, Z, M) :-
    plate(Z,M) &
    can_reach_agent(plate,Z,M) & 
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, Z, M, D) &
    not (plate(Z1, M1) &
         manhattan_distance(AX, AY, Z1, M1, D1) &
         D1 < D).

object(serve, Z, M) :-
    serve(Z, M) &
    can_reach_agent(serve,Z,M) &
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, Z, M, D) &
    not (serve(Z1, M1) &
         manhattan_distance(AX, AY, Z1, M1, D1) &
         D1 < D).

object(tomato, Z, M) :-
    tomato(Z, M) &
    can_reach_agent(tomato,Z,M).

object(onion, Z, M) :-
    onion(Z, M) &
    can_reach_agent(onion,Z,M) &
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
    can_reach_agent(pot,X,Y) &
    active_pot(X,Y) & 
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, X, Y, D) &
    not (active_pot(X1, Y1) &
         manhattan_distance(AX, AY, X1, Y1, D1) &
         D1 < D).

possible_counter(Z,M) :-
    can_reach_agent(counter,Z,M) &
    can_reach_human(counter,Z,M) &
    not placed_stuff(Z,M).

possible_counter_goto(Z, M) :-
    can_reach_agent(counter,Z,M) &
    not placed_stuff(Z,M).

object(possible_counter_togo,Z, M) :-
    can_reach_agent(counter,Z,M) &
    not placed_stuff(Z,M).


possible_counter_taken(Z,M) :-
    can_reach_agent(counter,Z,M) &
    can_reach_human(counter,Z,M) & 
    placed_stuff(Z,M).


closest_counter(Z,M) :-
    possible_counter(Z,M) &
    agent_position(Hx, Hy) &
    manhattan_distance(Hx, Hy, Z, M, D) &
    not ( possible_counter(Z1,M1)
        & manhattan_distance(Hx, Hy, Z1, M1, D1)
        & D1 < D ).

object(counterTo, Z, M) :-
    closest_counter(Z, M).


possible_counter_agent_only(Z,M) :-
    can_reach_agent(counter,Z,M) & not possible_counter(Z,M) & not placed_stuff(Z,M).

object(free_counter_agent,Z,M) :-
    possible_counter_agent_only(Z,M) &
    agent_position(Hx, Hy) &
    manhattan_distance(Hx, Hy, Z, M, D) &
    not ( possible_counter_agent_only(Z1,M1)
        & manhattan_distance(Hx, Hy, Z1, M1, D1)
        & D1 < D ).
    
placed_item(placed_onion, X, Y) :- placed_onion(X, Y).
placed_item(placed_tomato, X, Y) :- placed_tomato(X, Y).
placed_item(placed_soup, X, Y) :- placed_soup(X, Y).
placed_item(placed_dish, X, Y) :- placed_dish(X, Y).

select_pot(X,Y) :-
    pot(X,Y) &
    not (active_pot(X,Y)) &
    can_reach_agent(pot,X,Y).

placedObject(Thing,X,Y) :-
    placed_item(Thing,X,Y) &
    can_reach_agent(counter,X,Y).

 empty_pots :-
    not (pot_contents(X,Y, onion, OCount)) &
  not (pot_contents(X,Y, tomato, TCount)) &
  not (pot_contents(Z,M, onion, OCount)) &
  not (pot_contents(Z,M, tomato, TCount)).


can_reach_all :-
    can_reach_agent(plate,X,Y)  &
    can_reach_agent(serve,Z,M)  &
    can_reach_agent(pot,K,L)  &
    (can_reach_agent(onion,I,S)  |
    can_reach_agent(tomato,V,F)).

can_reach_all_human :-
    can_reach_human(plate,X,Y)  &
    can_reach_human(serve,Z,M)  &
    can_reach_human(pot,K,L)  &
    (can_reach_human(onion,I,S)  |
    can_reach_human(tomato,V,F)).

!start.


+!start : begin(now)
<- 

    ?recipes([FirstRecipe|_]);
    .print("Retrieved recipes: " , Recipes);
    ?count_objects(pot(_,_), Count);
    .wait(3000);
    .wait(300);
    !retrieve_current_recipe(2);
    //!collect_recipe(Count);
    //!can_reach_ingredients_pots(Count);
    //!test_route_cost;
    //!can_reach_pots_only(Count);
    //!can_reach_ingredients_only(1);
    //!can_reach_allfour(Count);
    //!can_reach_ingredients_plates;
    //!can_reach_ingredients_serve.
    //!can_reach_plates_pots(Count).
    //!can_reach_plates_serve.
    
    //!can_reach_ingpotserve(Count);
    //!can_reach_serveplpots(Count);
    //!can_reach_ingplpots(Count);
    //!can_reach_pots_serve(Count);

    //!can_reach_ingredients_only;
    //!can_reach_plates_only;
    //!can_reach_serve_only;
    !can_reach_pots_only(Count);
    //!test_case(Count).
    .print("All  plans have been executed. The agent can now rest :)").
 
 +!start : not begin(now)
<-  
    !start.

///// PLANS FOR THE AGENT
//
//////////////////////////////// 1 OBJECT ///////////////////////////////////////////////////
// When the agent can only reach 1 object - either onion/tomato, pot, serving station or a dish
// In this case, in order to succeed in making recipes, the human has to be collabarive as well
// in order to achieve a high amount of points.
//
//
// CASE: ONLY CAN REACH INGREDIENTS ////
// For this situation, the agent can only reach ingredients and ingredients only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Check for the current recipe how many ingredients it needs.
//      2) Pass each of them indiviually to the humans on counters shared by both the human and the agent. If there is no
//         free counters because the human is stalling, the agent will wait until one frees. The agent passes an ingredient
//         to its closest counter as it is more intuitive for the human to go where the agent is. Otherwise, as long as the 
//         human is moving, the agent will move too and not place down the ingredient.
//      3) This plan continues until all of the recipes from the Orders list have been served.
//      4) A current recipe in the order list is checked as folows:
//              -the plan calls an internal action that initially checks the first recipe (index N=1).
//              -The index gets incremented every time so that while the human is cooking a soup the agent can pass ingredients for
//               the recipes to follow.
//              -Had the human made a mistake on any of the orders, the agent will pass those ingredients again after passing ingredients for the 
//               other recipes first. For more info on this please check get_recipe_at_index.java
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_ingredients_only(N) : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & recipes([])
<-
    .print("Case: agent can only reach ingredients. All recipes have now been completed.").

// Recursive case: There is still orders to be made. In this case for each order we pass the ingredients to the human and
// do a call to the plan again so that we can proceed with the rest of the recipes.
+!can_reach_ingredients_only(N) : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & recipes(Recipes)
<-
    my.internal.actions.get_recipe_at_index(Recipes, N, CurrentRecipe);
    ?countInRecipe(onion, CurrentRecipe, OnionCount);
    ?countInRecipe(tomato, CurrentRecipe, TomatoCount);
    !pass_to_human(onion,OnionCount);
    !pass_to_human(tomato,TomatoCount);
    Nnew = N+1; 
    !can_reach_ingredients_only(Nnew). // Pass ingredients for the next recipe now

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_ingredients_only(N)
<-
    .print("Case: agent can only reach ingredients. This plan is not applicable here -> skipping it.").

// CASE: ONLY REACH PLATES
// For this case the agent can reach plates and plates only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Grab a plate for each recipe in the orders list and pass it to the human on a counter
//         that is reachable by both of them. To achieve this, the agent passes a plate once a soup has been
//         marked as "cooking" or "cooked" in the belief base.
//      2) If there is no free counters because the human is slow, the agent will wait until one frees.
//      3) This plan continues until all of the recipes from the orders list have been served.
//      4) For the case where a human is not cooperative/makes a mistake and serves a wrong order, the agent will simply
//         pass the more plates to the human so that they can plate the soups.
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_plates_only : can_reach_agent(plate,X,Y) & recipes([])
<-
    .print("Case: agent can only reach plates. All recipes have now been completed.").

// Recursive case: There is still orders to be made. In this case for each order we pass the plate to the human and
// do a call to the plan again so that we can proceed with the rest of the recipes. This plan executes if the
// soup is ready or cooking so that the agent doesn't pass too many plates, but can still account for mistakes on the human part
+!can_reach_plates_only : can_reach_agent(plate,X,Y) & recipes([FirstRecipe|_])  & (pot_cooking(Z,M) | pot_ready(Z,M))
<-
    .print("Getting a plate and passing it where the human can reach it.");
    !pass_to_human(plate,1);
    !can_reach_plates_only.

// Waiting case: If the human has not started cooking/hasn't cooked a soup yet there is no need to pass a plate yet so we wait 
+!can_reach_plates_only : can_reach_agent(plate,X,Y) & recipes([FirstRecipe|_])  
<-
    .print("The human does not need a plate yet.");
    .wait(4000);
    !can_reach_plates_only.
 
// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_plates_only
<-
    .print("Case: agent can only reach plates. This plan is not applicable here -> skipping it.").

// CASE: ONLY REACH SERVING LOCATIONS 
// For this case the agent can reach serving tiles and serving tiles only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Wait for the human to place a soup on their shared counters.
//      2) Grab the soup and serve it.
//      3) This plan continues until all of the recipes from the Orders list have been served.
//      4) If the human makes a mistake and hands over a soup with wrong ingredients, the agent will simply dispose of
//         it. Disposal is the same as serving but for a reward of 0 points.
// 
// NOTE: serving spots are also considered bins
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_serve_only : can_reach_agent(serve,X,Y) & recipes([])
<-
    .print("Case: agent can only reach serving tiles. All recipes have now been completed.").

// Recursive case: There is still orders to be made. In this case for each order we pick up a soup from the human (from 
// a shared counter) and do a call to the plan again so that we can proceed with the rest of the recipes.
+!can_reach_serve_only : can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_])
<-
    !pick_from_human(placed_soup,1,0,0,0);
    !go_to(serve);
    !can_reach_serve_only.

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_serve_only
<-
    .print("Case: agent can only reach serving tiles. This plan is not applicable here -> skipping it.").


// CASE: ONLY REACH POT
// For this case the agent can reach pots and  pots only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Wait for the human to place an ingredient of a recipe on their shared counters.
//      2) Grab the ingredient and place it in a pot if any of the pots 
//      3) Wait for the human to place a plate on the shared conuters. Grab the plate, pick up the soup and hand it back to the 
//         human (on the shared counters).
//      4) This plan continues until all of the recipes from the Orders list have been served.
//
// Special cases: human places ingredients that are not of this recipe on the shared counters -> the agent will pick them up
//              and place them on other counters to free up space. Consequtively, when making other recipes, they don't need to wait for
//              the human to place ingredients/plates on the shared counters as they can use those ones that have been previously "incorrect".
//
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_pots_only(Count) :  can_reach_agent(pot,X,Y) & recipes([])
<-
    .print("Case: agent can only reach pots. All recipes have now been completed.").

+!can_reach_pots_only(Count) :  active_pot(X,Y) & placed_dish(K,T) & pot_ready(X,Y) & object(counterTo,S,J)
<- 
    !pick_from_human(placed_dish,1,0,0,0);
    .wait(300);
    !go_to(active_pot,X,Y);
    !go_to(counterTo);
    .wait(400);
    !can_reach_pots_only(Count).

+!can_reach_pots_only(Count) : recipes(Recipes) & all_pots(used) & placed_onion(W,S) & active_pot(Z,M) & active_pot(J,D) & not object(counterTo,W,S) & not (Z = J & M = D)
<-
    .print(Z,M,J,D);
    my.internal.actions.get_pot([active_pot(Z,M),active_pot(J,D)], Recipes, onion, X, Y);
    
    !load(X,Y,placed_onion);
    .wait(500);
    !can_reach_pots_only(Count).

+!can_reach_pots_only(Count) : recipes(Recipes) & all_pots(used) & placed_tomato(W,S) & active_pot(Z,M) & active_pot(J,D) & not object(counterTo,W,S) & not (Z = J & M = D)
<-
    .print(Z,M,J,D);
    my.internal.actions.get_pot([active_pot(Z,M),active_pot(J,D)], Recipes, tomato, X, Y);
    !load(X,Y,placed_tomato);
    .wait(500);
    !can_reach_pots_only(Count).

+!can_reach_pots_only(Count) : recipes([FirstRecipe|_]) & not all_pots(used)
<-
    .print("Choosing pots to use:)");
    !choose_pot_for_recipe(FirstRecipe, Count);
    !can_reach_pots_only(Count).

+!can_reach_pots_only(Count) : recipes([FirstRecipe|_])
<-
    .wait(500);
    .print("waiting");
    !can_reach_pots_only(Count).

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_pots_only(Count)
<-
    .print("Case: agent can only reach pots. This plan is not applicable here -> skipping it.").

+!load(X,Y,Thing) : pot_to_cook(X,Y)
<-
    !go_to(Thing);
    !go_to(active_pot,X,Y);
    !execute;
    remove_beliefs(pot_to_cook(X,Y)).

+!load(X,Y,Thing) : X < 888 & Y < 888
<-
    !go_to(Thing);
    .wait(300);
    !go_to(active_pot,X,Y).

+!load(X,Y,Thing)
<-
    .print("This ingredient is not compatible with any pot");
    !go_to(Thing);
    !go_to(free_counter_agent).






















//////////////////////////////// 2 OBJECTS ///////////////////////////////////////////////////
// When the agent can only reach 2 objects
// In this case, in order to succeed in making recipes, the human has to be collabarive as well
// in order to achieve a high amount of points.
//
//
// CASE: ONLY CAN REACH INGREDIENTS + PLATES ////
+!can_reach_ingredients_plates : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & recipes([])
<-
    .print("Case: agent can only reach ingredients and plates. All recipes have now been completed.").

+!can_reach_ingredients_plates : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, NOnion);
    ?countInRecipe(tomato, FirstRecipe, NTomato);
    !pass_to_human(onion,NOnion);
    !pass_to_human(tomato,NTomato);
    .wait(400);
    !pass_to_human(plate,1);
    .wait(1000); // PASS THE NEXT RECIPE
    !can_reach_ingredients_plates.

+!can_reach_ingredients_plates
<- 
    .print("Case: agent can only reach ingredients and plates. This plan is not applicable here -> skipping it.").

// CASE: ONLY CAN REACH INGREDIENTS + POTS ////
+!can_reach_ingredients_pots(Count) : recipes([])
<-
    .print("Case: agent can only reach ingredients and pots. All recipes have now been completed.").

+!can_reach_ingredients_pots(Count) : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(pot,X,Y) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, NOnion);
    ?countInRecipe(tomato, FirstRecipe, NTomato);
    !choose_pot_for_recipe(FirstRecipe,Count);
    !pick_load(onion,NOnion,NOnion,X,Y);
    !pick_load(tomato,NTomato,NTomato,X,Y);
    !execute;
    .wait(400);
    !pick_from_human(placed_dish,1,0,0,0);
    !pass_to_human(placed_soup,1);
    remove_beliefs(active_pot(_,_));
    .wait(3000); // PASS THE NEXT RECIPE
    !can_reach_ingredients_pots(Count).

+!can_reach_ingredients_pots(Count)
<- 
    .print("Case: agent can only reach ingredients and pots. This plan is not applicable here -> skipping it.").


// CASE: ONLY CAN REACH PLATES + POTS ////
+!can_reach_plates_pots(Count) : recipes([])
<-
    .print("done").

+!can_reach_plates_pots(Count) : can_reach_agent(plate,Z,M) & can_reach_agent(pot,X,Y) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, OnionCount);
    ?countInRecipe(tomato, FirstRecipe, TomatoCount);
    !choose_pot_for_recipe(FirstRecipe,Count);
    !pick_from_human(placed_onion,OnionCount,1,X,Y);
    !pick_from_human(placed_tomato,TomatoCount,1,X,Y);
    !execute;
    .wait(4000);
    !go_to(plate);
    !pass_to_human(placed_soup,1);
    remove_beliefs(active_pot(_,_));
    .wait(3000);
    !can_reach_plates_pots(Count).

+!can_reach_plates_pots(Count)
<-
    .print("Case: agent can only reach plates and pots. This plan is not applicable here -> skipping it.").

// CASE: ONLY CAN REACH PLATES + SERVING LOCATIONS ////
+!can_reach_plates_serve : recipes([])
<-
    .print("Case: agent can only reach serving locations and plates. This plan is not applicable here -> skipping it.").

+!can_reach_plates_serve : can_reach_agent(plate,Z,M) & can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_]) & placed_soup(L,K) & possible_counter_taken(L,K)
<-
    !pick_from_human(placed_soup,1,0,0,0);
    !go_to(serve);
    !can_reach_plates_serve.

+!can_reach_plates_serve : can_reach_agent(plate,Z,M) & can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_])
<-
    !pass_to_human(plate,1);
    .wait(4000);
    !can_reach_plates_serve.

+!can_reach_plates_serve
<-
    .print("Case: agent can only reach plates and serving locations. This plan is not applicable here -> skipping it.").

// CASE: ONLY CAN REACH POTS + SERVING LOCATIONS ////
+!can_reach_pots_serve(Count) : recipes([])
<-
    .print("Case: agent can only reach serving locations and plates. This plan is not applicable here -> skipping it.").


+!can_reach_pots_serve(Count) : can_reach_agent(pot,Z,M) & can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_]) & active_pot(Z,M)
<-
    ?countInRecipe(onion, FirstRecipe, OnionCount);
    ?countInRecipe(tomato, FirstRecipe, TomatoCount);
    !choose_pot_for_recipe(FirstRecipe,Count);
    !pick_from_human(placed_onion,OnionCount,1,Z,M);
    !pick_from_human(placed_tomato,TomatoCount,1,Z,M);
    !execute;
    !pick_from_human(placed_dish,1,0,0,0);
    .wait(2000);
    !go_to(active_pot,Z,M);
    remove_beliefs(active_pot(_,_));
    !go_to(serve);
    .wait(400);
    !can_reach_pots_serve(Count).

+!can_reach_pots_serve(Count)
<-
    .print("Case: agent can only reach plates and serving locations. This plan is not applicable here -> skipping it.").


// CASE: ONLY CAN REACH INGREDIENTS + SERVING LOCATIONS ////
+!can_reach_ingredients_serve : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y) & recipes([])
<-
    .print("Case: agent can only reach ingredients and serving locations. All recipes have now been completed.").


+!can_reach_ingredients_serve : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_]) & placed_soup(K,L)
<-
    !pick_from_human(placed_soup,1,0,0,0);
    !go_to(serve);
    !can_reach_ingredients_serve.

+!can_reach_ingredients_serve : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, NOnion);
    ?countInRecipe(tomato, FirstRecipe, NTomato);
    !pass_to_human(onion,NOnion);
    !pass_to_human(tomato,NTomato);
    !can_reach_ingredients_serve.

+!can_reach_ingredients_serve
<- 
    .print("Case: agent can only reach ingredients and serving locations. This plan is not applicable here -> skipping it.").



//////////////////////////////// 3 OBJECTS ///////////////////////////////////////////////////
// When the agent can only reach 3 objects
// In this case, in order to succeed in making recipes, the human has to be collabarive as well
// in order to achieve a high amount of points.
//
//
// CASE: ONLY CAN REACH INGREDIENTS + PLATES + SERVING LOCATIONS ////
+!can_reach_ingplserve : recipes([])
<-
    .print("Case: agent can only reach ingredients, plates and serving locations. All recipes have now been completed.").

+!can_reach_ingplserve : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & can_reach_agent(serve,V,S) & recipes([FirstRecipe|_]) & placed_soup(I,B) & possible_counter_taken(I,B)
<-
    !pick_from_human(placed_soup,1,0,0,0);
    !go_to(serve);
    remove_beliefs(active_pot(_,_));
    .wait(500);
    !can_reach_ingplserve.

+!can_reach_ingplserve : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & can_reach_agent(serve,V,S) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, OnionCount);
    ?countInRecipe(tomato, FirstRecipe, TomatoCount);
    !pass_to_human(onion,OnionCount);
    !pass_to_human(tomato,TomatoCount);
    !pass_to_human(plate,1);
    remove_beliefs(active_pot(_,_));
    .wait(400);
    !can_reach_ingplserve.

+!can_reach_ingplserve
<-
    .print("Case: agent can only reach ingredients, plates and pots. This plan is not applicable here -> skipping it.").

// CASE: ONLY CAN REACH INGREDIENTS + PLATES + POTS //
+!can_reach_ingplpots(Count) : recipes([])
<-
    .print("Case: agent can only reach ingredients, plates and potss. All recipes have now been completed.").



+!can_reach_ingplpots(Count) : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & can_reach_agent(pot,V,S) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, OnionCount);
    ?countInRecipe(tomato, FirstRecipe, TomatoCount);
    !choose_pot_for_recipe(FirstRecipe,Count);
    !pick_load(onion,OnionCoun,OnionCount,V,S);
    !pick_load(tomato,TomatoCount,TomatoCount,V,S);
    !execute;
    .wait(4000);
    !go_to(plate);
    !pass_to_human(placed_soup,1);
    remove_beliefs(active_pot(_,_));
    .wait(4000);
    !can_reach_ingplpots(Count).

+!can_reach_ingplpots(Count)
<-
    .print("Case: agent can only reach ingredients, plates and pots. This plan is not applicable here -> skipping it.").

// CASE: ONLY CAN REACH INGREDIENTS + POTS + SERVING LOCATIONS ////
+!can_reach_ingpotserve(Count) : recipes([])
<-
    .print("Case: agent can only reach ingredients, serving locations and pots. All recipes have now been completed.").


+!can_reach_ingpotserve(Count) : can_reach_agent(pot,X,Y) & can_reach_agent(serve,Z,M) & (can_reach_agent(onion,K,L) | can_reach_agent(tomato,P,T)) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, OnionCount);
    ?countInRecipe(tomato, FirstRecipe, TomatoCount);
    !choose_pot_for_recipe(FirstRecipe,Count);
    .wait(300);
    !pick_load(onion,OnionCount,OnionCount,X,Y);
    !pick_load(tomato,TomatoCount,TomatoCount,X,Y);
    !execute;
    !pick_from_human(placed_dish,1,0,0,0);
    !go_to(active_pot,X,Y);
    !go_to(serve);
    .wait(600);
    remove_beliefs(active_pot(X,Y));
    !can_reach_ingpotserve(Count).

+!can_reach_ingpotserve(Count)
<-
    .print("Case: agent can only reach ingredients, serving locations and pots. This plan is not applicable here -> skipping it.").

// CASE: ONLY CAN REACH SERVING LOCATIONS + PLATES + POTS ////
+!can_reach_serveplpots(Count) : recipes([])
<-
    .print("Case: agent can only reach plates, serving locations and pots. All recipes have now been completed.").

+!can_reach_serveplpots(Count) : can_reach_agent(serve,X,Y) & can_reach_agent(plate,Z,M) & can_reach_agent(pot,K,L) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, OnionCount);
    ?countInRecipe(tomato, FirstRecipe, TomatoCount);
    !choose_pot_for_recipe(FirstRecipe,Count);
    !pick_from_human(placed_onion,OnionCount,1,K,L);
    !pick_from_human(placed_tomato,TomatoCount,1,K,L);
    !execute;
    .wait(3000);
    !go_to(plate);
    !go_to(active_pot,K,L);
    !go_to(serve);
    remove_beliefs(active_pot(_,_));
    .wait(500);
    !can_reach_serveplpots(Count).

+!can_reach_serveplpots(Count)
<-
    .print("Case: agent can only reach serving locations, plates and pots. This plan is not applicable here -> skipping it.").
/////////////////////////////////////////////////
//////////////
///////////////

+!can_reach_allfour(Count) : recipes([])
<-
    .print("Case: agent can reach everything. All recipes have now been completed.").



+!can_reach_allfour(Count) : can_reach_all & can_reach_all_human & active_pot(X,Y) & recipes([FirstRecipe|_])
  
 <-
    ?countInRecipe(onion, FirstRecipe, OnionCount);
    ?countInRecipe(tomato, FirstRecipe, TomatoCount);
    .wait(400);
    !pick_load(onion,OnionCount,OnionCount,X,Y);
    !pick_load(tomato,TomatoCount,TomatoCount,X,Y);
    !execute;
    .wait(4000);
    !go_to(plate);
    .wait(300);
    !go_to(active_pot,X,Y);
    .wait(400);
    !go_to(serve);
    remove_beliefs(active_pot(X,Y));
    !can_reach_allfour(Count).




+!can_reach_allfour(Count) : can_reach_all & can_reach_all_human & recipes([FirstRecipe|_])
<-
    // Choose a pot
    .print("here");
    !choose_pot_for_recipe(FirstRecipe,Count);
    !can_reach_allfour(Count).

+!can_reach_allfour(Count)
<-
    .print("Case: agent cannot reach everything. This plan is not applicable here -> skipping it.").
    


///// HELPER PLANS FOR THE AGENT: Used by the agent when executing the other plans
//
//////////////////////////////// GO TO ///////////////////////////////////////////////////
// When the agent wants to go somewhere it will call this plan which:
// 1) Checks if the agent is already at the desired location. In that case,
//   it calls the turn_to helper plan, which will turn the agent towards the object they want to interact with.
// 2) If the agent is not adjacent to the desired location, the plan will use an external 
//    action - compute_path, which uses the A* algorithm to find an optimal path to reach 
//    the desired location of the agent. 
// Human collision (the human going into the agent's way) is accounted for in the Java environment
//



+!go_to(Object,Z,M) :  agent_position(X,Y) & object(Object,Z,M) & adjacent(X,Y,Z,M)
<- 
    .print("Go to helper function: the agent has reached the desired destination.");
    .wait(300);
    !turn_to(Object).
// Base case: The agent has arrived at the desired location and therefore we can exit the plan by calling turn_to.
+!go_to(Object) :  agent_position(X,Y) & object(Object,Z,M) & adjacent(X,Y,Z,M)
<- 
    .print("Go to helper function: the agent has reached the desired destination.");
    .wait(300);
    !turn_to(Object).



// Recursive case: The agent is not at the desired location, so it calls the A* algorithm to compute a path for it
// After that it calls go_to so that the base case can be reached.
+!go_to(Object,Z,M) : agent_position(X,Y)
<-
    .print("The agent is going to:", object(Object, Z,M));
    compute_path(Z,M);
    !go_to(Object,Z,M).


+!go_to(Object) : agent_position(X,Y) & object(Object,Z,M) & not adjacent(X,Y,Z,M) 
<-
    .print("The agent is going to:", object(Object, Z,M));
    compute_path(Z,M);
    !go_to(Object).

// Fail condition: if the plan has executed too quickly and failed, the plan will be called again until successful computation
+!go_to(Object)
<-
    .print("Going to: ", Object," failed. Trying again...");
    !go_to(Object).

+!go_to(Object,Z,M)
<-
    .print("Going to: ", Object," at (", Z,", ",M,") failed. Trying again...");
    !go_to(Object,Z,M).

//////////////////////////////// TURN TO ///////////////////////////////////////////////////
// When the agent wants to turn to an object it will call this plan which:
// - Based on the coordinates of the agent and the object will decide which direction the agent has to turn towards
// - This is needed as in order to interact with pots, plates, etc., the agent must be facing them.
// - The plan additionally calls the !execute plan which is responsible for interaction with the objects.
//
// NB: the layout's coordinates are as follows:
// Top left corner: (0,0)
// Bottom left corner: (O,Y)
// Top right corner: (X,0)
// Bottom right corner: (X,Y)
//
//
// Right: the agent's X coordinate is smaller than the object's X coordinate which means the object is on the agent's right side
+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (X < Z)
<- 
    action("right");
    .wait(300);
    !execute.

// Up: the agent's Y coordinate is greater than the object's Y coordinate which means the object is above the agent 
+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (Y > M) 
<- 
    action("up");
    .wait(300);
    !execute.

// Down: the agent's Y coordinate is smaller than the object's Y coordinate which means the object is below the agent 
+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (Y < M)
<- 
    action("down");
    .wait(300);
    !execute.

// Left: the agent's X coordinate is greater than the object's X coordinate which means the object is on the agent's left side
+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (X > Z)
<- 
    action("left");
    .wait(300);
    !execute.

 // Fail condition: if the plan has executed too quickly and failed, the plan will be called again until successful computation
+!turn_to(Object)
<-
    .print("Turning to: ", Object," failed. Trying again...");
    !turn_to(Object).

//////////////////////////////// EXECUTE ///////////////////////////////////////////////////
// This helper function is used for more clarity in the execution of other plans. It simply
// calls the external action "space" which is responsible for interaction with objects.

// Only case: this function will always execute as there is no preconditions needed for interactions as long as we have reached 
// the desired location in the previous plans.
+!execute : true
<-
    action("space").


+!pick_load(Thing,N,NOriginal,X,Y) : N = 0
<-
    !go_to(active_pot,X,Y);
    .print("done picking").

+!pick_load(Think,N,NOriginal,X,Y) : discard_recipe(X,Y)
<-
    !discard_soup(X,Y);
    remove_beliefs(discard_recipe(X,Y));
    !pick_load(Thing,Noriginal,Noriginal).

+!pick_load(Thing,N,NOriginal,X,Y) : N>0 & active_pot(X,Y) & recipes_new([FirstRecipe|_]) & countInRecipe(onion, FirstRecipe, Number1) &  countInRecipe(tomato, FirstRecipe, Number2) & Number1+Number2 = 0
<-
    .print("CC");
    remove_beliefs(recipes_new(_));
    !go_to(active_pot,X,Y).

+!pick_load(Thing,N,NOriginal,X,Y) : N>0 & active_pot(X,Y) & recipes_new([FirstRecipe|_]) 
<-
    .print("C");
    remove_beliefs(recipes_new(_));
    ?countInRecipe(Thing, FirstRecipe, Number);
    .print("found ",Thing);
    .wait(300);
    !pick_load(Thing,Number,Number).

+!pick_load(Thing,N,NOriginal,X,Y) : N > 0 & active_pot(X,Y) & pot(matches_recipe)
<-
    .print("B");
    remove_beliefs(pot(matches_recipe));
    !go_to(Thing);
    !go_to(active_pot,X,Y);
    .wait(400);
    Nnew = N -1 ;
    .wait(300);
    !pick_load(Thing,Nnew,NOriginal,X,Y).

+!pick_load(Thing,N,NOriginal,X,Y) : N > 0 & active_pot(X,Y)
<- 
     match_pot_with_recipe(X,Y);
     .print("A");
    !pick_load(Thing,N,NOriginal,X,Y).

// PICKING STUFF UP FROM HUMAN
+!pick_from_human(Thing,0,M,Z,J)
<-
    .print("All things picked up from the counter").

+!pick_from_human(Thing,N,M,Z,J) :  placedObject(Thing,X,Y) & N>0 & M = 0
// M = 0 means its not a pot we are going to
<-
    !go_to(Thing);
    Nnew = N - 1;
    !pick_from_human(Thing,Nnew,M,Z,J).

+!pick_from_human(Thing,N,M,Z,J) :  placedObject(Thing,X,Y) & N>0 
<-
    
    !go_to(Thing);
    .wait(300);
    !go_to(active_pot,Z,J);
    Nnew = N - 1;
    !pick_from_human(Thing,Nnew,M,Z,J).


+!pick_from_human(Thing,N,M,Z,J) :  placedObject(OtherThing,X,Y) & not (placed_soup(X,Y)) & N>0 & possible_counter_taken(X,Y)
<-
    !go_to(OtherThing);
    .print("from here");
    .wait(300);
    !go_to(free_counter_agent);
    !pick_from_human(Thing,N,M,Z,J).

+!pick_from_human(Thing,N,M,Z,J)
<-
    .print("waiting for object to be passed from human");
    .wait(500);
    .print(Thing);
    !pick_from_human(Thing,N,M,Z,J).
/////////////
// PASSING STUFF WHEN THERE IS A TABLE/STH ELSE IN BETWEEN
+!pass_to_human(Ingredient,0)
<-
    .print("All things passed successfully").


+!pass_to_human(Ingredient,N) : (object(placed_soup,Z,M) | can_reach_agent(Ingredient,Z,M)) & object(counterTo,X,Y) & N>0
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
// if it was one pot that did not match the recipe

// one empty pot
+!choose_pot_for_recipe(Recipe, Count) : pot(X,Y)  & Count == 1 & not (active_pot(X,Y))
<-
    .print("only one pot -> using it");
    add_beliefs(active_pot(X,Y)).

+!choose_pot_for_recipe(Recipe,Count) : pot(X,Y) & Count == 1 
<-
    add_beliefs(all_pots(used));
.print("all pots currently in use").

+!choose_pot_for_recipe(Recipe, Count) : Count=2 & select_pot(X,Y)
<-
.print("active pot is now selected");
  add_beliefs(active_pot(X,Y)).

+!choose_pot_for_recipe(Recipe, Count) : Count=2 
<-
add_beliefs(all_pots(used));
.print("all pots currently in use").

+!choose_pot_for_recipe(Recipe,Count)
<-
    .print("plan failed to pick a pot ",Count).



+!discard_soup(Z,M)
<-
    //using compute path here as we need to go to that specific pot
    compute_path(Z,M);
    !turn_to(pot);
    !go_to(plate);
    .wait(5000);
    compute_path(Z,M);
    !turn_to(pot);
    !go_to(serve).
    
//+!wait_for_soup : pot_ready(X,Y)
//<-
  //  .print("soup is ready to be picked up").


+!wait_for_soup
<-
    .print("waiting for soup to cook");
    .wait(5000).
    //!wait_for_soup.



//////////////////////////////// TESTS ///////////////////////////////////////////////////
+!test_route_cost
<-
    ?route_cost(onion, CostOnion);
    ?route_cost(plate, CostPlate);
    .print("The onion is ,",CostOnion," tiles away");
    .print("The plate is ,",CostPlate," tiles away").



// Test for retrieving the current recipe and its ingredients. It checks two things: 
// - test the call to the internal function.
// - test whether the countInRecipe rule countes the number of ingredients in a recipe correctly.
// These print out as follows in the MAS console;
//   Current recipe at index N is: [ingredient 1, ingredient 2,..., ingredient N]
//   This recipe contains N onions and M tomatoes.
// NB: If you entered an index out of range, e.g. 500, the internal action will return the first
//     recipe in the orders list.

+!retrieve_current_recipe(N) : recipes(Recipes)
<- 
   my.internal.actions.get_recipe_at_index(Recipes, N, CurrentRecipe);
   ?countInRecipe(onion, CurrentRecipe, OnionCount);
   ?countInRecipe(tomato, CurrentRecipe, TomatoCount);
   .print("Current recipe at index ", N, " is: ", CurrentRecipe);
   .print("This recipe contains ", OnionCount, " onions and ",TomatoCount, " tomatoes.").