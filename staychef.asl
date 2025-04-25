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
    counter(Z,M) & 
    not tomato(Z,M) &
    not onion(Z,M) &
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
    .wait(500);
    ?recipes([FirstRecipe|_]);
    ?count_objects(pot(_,_), Count);
    .print(Count);
    .wait(300);
    
    !can_reach_allfour(1);
    
   
    // Three objects:
    !can_reach_ingplserve(1);
    !can_reach_ingplpots(1);
    !can_reach_ingpotserve(1);
    .print("heres");
    !can_reach_serveplpots;

    // Two objects:
    !can_reach_ingredients_plates(1);
    !can_reach_ingredients_pots(1);
    !can_reach_plates_pots;
    !can_reach_plates_serve;
    !can_reach_pots_serve;
    !can_reach_ingredients_serve(1);

    // Single object
    !can_reach_ingredients_only(1);
    !can_reach_plates_only;
    !can_reach_serve_only;
    !can_reach_pots_only;
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
+!can_reach_ingredients_only(N) : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & cannot_reach_agent(serve,A,B) & cannot_reach_agent(plate,C,D)
& cannot_reach_agent(pot,E,F) & recipes([])
<-
    .print("Case: agent can only reach ingredients. All recipes have now been completed.").

// Recursive case: There is still orders to be made. In this case for each order we pass the ingredients to the human and
// do a call to the plan again so that we can proceed with the rest of the recipes.
+!can_reach_ingredients_only(N) : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & recipes(Recipes) & cannot_reach_agent(serve,A,B) & cannot_reach_agent(plate,C,D)
& cannot_reach_agent(pot,E,F)
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
+!can_reach_plates_only : can_reach_agent(plate,X,Y) & recipes([]) & cannot_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D)
<-
    .print("Case: agent can only reach plates. All recipes have now been completed.").

// Recursive case: There is still orders to be made. In this case for each order we pass the plate to the human and
// do a call to the plan again so that we can proceed with the rest of the recipes. This plan executes if the
// soup is ready or cooking so that the agent doesn't pass too many plates, but can still account for mistakes on the human part
+!can_reach_plates_only : can_reach_agent(plate,X,Y) & recipes([FirstRecipe|_])  & pot_ready(A,B) & cannot_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D)
<-
    .print("Getting a plate and passing it where the human can reach it.");
    !pass_to_human(plate,1);
    remove_beliefs(pot_ready(A,B));
    !can_reach_plates_only.

// Waiting case: If the human has not started cooking/hasn't cooked a soup yet there is no need to pass a plate yet so we wait 
+!can_reach_plates_only : can_reach_agent(plate,X,Y) & recipes([FirstRecipe|_])  & cannot_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D)
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
+!can_reach_serve_only : can_reach_agent(serve,X,Y) & recipes([]) & cannot_reach_agent(pot,A,B) & cannot_reach_agent(plate,C,D)
<-
    .print("Case: agent can only reach serving tiles. All recipes have now been completed.").

// Recursive case: There is still orders to be made. In this case for each order we pick up a soup from the human (from 
// a shared counter) and do a call to the plan again so that we can proceed with the rest of the recipes.
+!can_reach_serve_only : can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_]) & cannot_reach_agent(pot,A,B) & cannot_reach_agent(plate,C,D)
& placed_soup(W,S) & can_reach_agent(counter,W,S)
<-
    !go_to(placed_soup);
    !go_to(serve);
    !can_reach_serve_only.

// Waiting: wait until a soup is passed by the human
+!can_reach_serve_only : can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_]) & cannot_reach_agent(pot,A,B) & cannot_reach_agent(plate,C,D)
<-
    .print("Waiting fo the human to pass a soup on a shared counter");
    .wait(1000);
    !can_reach_serve_only.

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_serve_only
<-
    .print("Case: agent can only reach serving tiles. This plan is not applicable here -> skipping it.").


// CASE: ONLY REACH POT
// For this case the agent can reach pots and  pots only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Wait for the human to place an ingredient of a recipe on their shared counters.
//      2) Grab the ingredient and check if it can be placed in any of the pots. For example:
//          - we have two pots - one empty and one containing a single tomato.
//          - we have the following orders: [[onion,onion,onion],[tomato,tomato],[onion]]
//          - we pass a tomato to the agent. It matches the second recipe and the second pot.
//          - the agent places it in the pot, and starts cooking the soup
//          - if the ingredient did not match any pots and recipes, it would have been grabbed by 
//              the agent and moved to another counter (not a shared one) to free up space on the shared counters.
//      3) Wait for the human to place a plate on the shared counters. Grab the plate, pick up the soup and hand it back to the 
//         human (on the shared counters). If the human does not pass a plate, just keep loading other pots with ingredients. And
//         if the human does not do anything, just keep waiting until they do.
//      4) This plan continues until all of the recipes from the Orders list have been served.
//
// Special cases: human places ingredients that are not of this recipe on the shared counters -> the agent will pick them up
//              and place them on other counters to free up space. Consequtively, when making other recipes, they don't need to wait for
//              the human to place ingredients/plates on the shared counters as they can use those ones that have been previously "incorrect".
//
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_pots_only(Count) :  can_reach_agent(pot,X,Y) & recipes([]) & cannot_reach_agent(serve,A,B) & cannot_reach_agent(plate,C,D)
<-
    .print("Case: agent can only reach pots. All recipes have now been completed.").

// Prioritised goal: if we pass a plate to the agent and there is a soup that is already cooked -> grab the plate, plate the soup and pass it back on the shared counter
+!can_reach_pots_only(Count) :  active_pot(X,Y) & placed_dish(K,T) & can_reach_agent(counter,K,T) & pot_ready(X,Y) & object(counterTo,S,J) &  can_reach_agent(pot,X,Y) & cannot_reach_agent(serve,A,B) & cannot_reach_agent(plate,C,D)
<- 
    !go_to(placed_dish);
    !go_to(active_pot,X,Y);
    !go_to(counterTo);
    .wait(400);
    !can_reach_pots_only(Count).

// Case 1: we passed an onion to the agent -> check if it can be placed in any pot by using the internal function. If not, move the onion away from the counter to free up space (this
// happens in the "load" plan).
+!can_reach_pots_only(Count) : recipes(Recipes) & all_pots(used) & placed_onion(W,S) & can_reach_agent(counter,W,S)   & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) 
& can_reach_agent(pot,A,B) & cannot_reach_agent(serve,Q,L) & cannot_reach_agent(plate,C,D)
  & not (AllPots = [] )
<-
    my.internal.actions.get_pot(AllPots, Recipes, onion, X, Y);
    !load(X,Y,placed_onion);
    .wait(500);
    !can_reach_pots_only(Count).

// Case 2: we passed an tomato to the agent -> check if it can be placed in any pot by using the internal function. If not, move the tomato away from the counter to free up space (this
// happens in the "load" plan).
+!can_reach_pots_only : recipes(Recipes) & all_pots(used) & placed_tomato(W,S) & can_reach_agent(counter,W,S)  & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots)
  &  can_reach_agent(pot,A,B) & not ( AllPots = [] ) & cannot_reach_agent(serve,Q,L) & cannot_reach_agent(plate,C,D)
<-
    my.internal.actions.get_pot(AllPots, Recipes, tomato, X, Y);
    !load(X,Y,placed_tomato);
    .wait(500);
    !can_reach_pots_only.

// First goal: "activate" all of the pots by calling choose_pot_for recipe so that we can keep track of pots by adding them in the belief base
+!can_reach_pots_only : recipes([FirstRecipe|_]) & not all_pots(used) & can_reach_agent(pot,K,T)  & cannot_reach_agent(serve,Q,L) & cannot_reach_agent(plate,C,D)
<-
    .print("Activating pots...");
    !choose_pot_for_recipe;
    !can_reach_pots_only.

// Waiting: the human is not passing anything on the counter and the agent has done all their current tasks -> wait until human passes stuff
+!can_reach_pots_only : recipes([FirstRecipe|_])  & can_reach_agent(pot,K,T)  & cannot_reach_agent(serve,Q,L) & cannot_reach_agent(plate,C,D)
<-
    .wait(500);
    .print("Waiting for human to place ingredients or plates.");
    !can_reach_pots_only.

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_pots_only
<-
    .print("Case: agent can only reach pots. This plan is not applicable here -> skipping it.").



//////////////////////////////// 2 OBJECTS ///////////////////////////////////////////////////
// When the agent can only reach 2 objects
// In this case, in order to succeed in making recipes, the human has to be collabarive as well
// in order to achieve a high amount of points.
//
//
// CASE: ONLY CAN REACH INGREDIENTS + PLATES ////
// For this case the agent can reach plates and  ingredients only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Alternate between tomatoes and onions to:
//          -check if an ingredient can be placed in any of the pots using get_pot internal action just like in can_reach_pots_only
//          -if yes, it will psas the ingredient to the human and if not it will simply ignore it
//      2) If a soup becomes cooked, the agent will pass a plate to the human so that they can serve it.
// The plan will continue executing untill all recipes have been completed. For the case there is no free counters to pass ingredients/plates,
// the agent will wait for the human to pick them up.
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_ingredients_plates(N) : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & recipes([]) & cannot_reach_agent(pot,A,B)
& cannot_reach_agent(serve,C,D)
<-
    .print("Case: agent can only reach ingredients and plates. All recipes have now been completed.").

// Prioritised task: if a soup is cooked, grab a plate and pass it to the human 
+!can_reach_ingredients_plates(N) :  can_reach_agent(plate,X,Y) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & recipes([FirstRecipe|_]) 
& pot_ready(A,B) & cannot_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D) & object(counterTo,W,S)
<-
    .print("Getting a plate and passing it where the human can reach it.");
    !go_to(plate);
    !go_to(counterTo);
    remove_beliefs(pot_ready(A,B));
    .wait(300);
    !can_reach_ingredients_plates(N).

// Onions: if we pass an onion can the human place it in any of the pots? if yes, pass it and if not - ignore (handled in the "pass" plan)
+!can_reach_ingredients_plates(N) : all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & recipes(Recipes)
& .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) & N=1
& cannot_reach_agent(pot,Q,R) & cannot_reach_agent(serve,C,D)  & object(counterTo, W,S)
<-
    my.internal.actions.get_pot(AllPots, Recipes, onion, A,B);
    !pass(A,B,onion);
    .wait(200);
    Nnew=N+1; // now try to pass a tomato
    !can_reach_ingredients_plates(Nnew).

// Tomatoes: if we pass an onion can the human place it in any of the pots? if yes, pass it and if not - ignore (handled in the "pass" plan)
+!can_reach_ingredients_plates(N) : all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & recipes(Recipes)
& .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) & N=2
& cannot_reach_agent(pot,Q,R) & cannot_reach_agent(serve,C,D) & object(counterTo, W,S)
<-
    my.internal.actions.get_pot(AllPots, Recipes, tomato, A,B);
    !pass(A,B,tomato);
    .wait(200);
    Nnew=N-1; // now try to pass an onion
    !can_reach_ingredients_plates(Nnew).

// First goal: "activate" all of the pots by calling choose_pot_for recipe so that we can keep track of pots by adding them in the belief base
+!can_reach_ingredients_plates(N) : recipes([FirstRecipe|_]) & not all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y)
& cannot_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D)
<-
    .print("Activating pots...");
    !choose_pot_for_recipe;
    .wait(400);
    !can_reach_ingredients_plates(N).

// Waiting: there is not free counter so we wait
+!can_reach_ingredients_plates(N) : all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y)
& cannot_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D)
<- 
    .wait(200);
    .print("Waiting for a free counter...");
    !can_reach_ingredients_plates(N).

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_ingredients_plates(N)
<- 
    .print("Case: agent can only reach ingredients and plates. This plan is not applicable here -> skipping it.").

// CASE: ONLY CAN REACH INGREDIENTS + POTS ///
// For this case the agent can reach ingredients and pots only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Alternate between tomatoes and onions to:
//          -check if an ingredient can be placed in any of the pots using get_pot internal action just like in can_reach_pots_only
//          -if yes, it will place the ingredient into the pot and if not it will simply ignore it
//          -if that was the last ingredient for a soup, the agent will also start cooking it
//      2) If a soup becomes cooked, and the human has passed a plate to the agent, the agent will plate the soup and pass it to the human so that they can serve it.
// The plan will continue executing until all recipes have been completed. For the case that all pots have been used and the agent does not have a dish to plate them,
// it will wait for the human to pass one.
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_ingredients_pots(N) : recipes([]) & (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & cannot_reach_agent(plate,X,Y) & can_reach_agent(pot,A,B)
& cannot_reach_agent(serve,C,D)
<-
    .print("Case: agent can only reach ingredients and pots. All recipes have now been completed.").

// Prioritised task: if a soup is cooked, grab a plate and pass it to the human 
+!can_reach_ingredients_pots(N) :  placed_dish(S,J) & can_reach_agent(counter,S,J) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) 
& can_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D) & cannot_reach_agent(plate,W,Q) & recipes([FirstRecipe|_])  & pot_ready(A,B) & object(counterTo,Y,I)
<-
    .print("Getting a plate and plating the soup.");
    !go_to(placed_dish);
    !go_to(active_pot,A,B);
    !go_to(counterTo);
    remove_beliefs(pot_ready(A,B));
    !can_reach_ingredients_pots(N).

// Move on: if a recipe was cooked, go on to the next recipe from the orders list
+!can_reach_ingredients_pots(N) : increment(n) & (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & 
can_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D) & cannot_reach_agent(plate,E,F) 
& recipes(Recipes) & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) 
<-
    remove_beliefs(increment(n));
    Nnew = N+1;
    .wait(300);
    !can_reach_ingredients_pots(Count,Nnew).

// Load ingredients: if a tomato or onion can be placed in any of the pots and be compatible with a recipe -> load it, otherwise do nothing
+!can_reach_ingredients_pots(N) : all_pots(used) & (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & recipes(Recipes) 
& can_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D) & cannot_reach_agent(plate,E,F) 
& .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) 
<-
   
    my.internal.actions.get_recipe_at_index(Recipes, N, CurrentRecipe);
    my.internal.actions.get_pot(AllPots, [CurrentRecipe], onion, A1,B1);
    !load(A1,B1,onion);
    .wait(500);
    my.internal.actions.get_pot(AllPots, [CurrentRecipe], tomato, A2,B2);
    !load(A2,B2,tomato); 
    .wait(500);
    !can_reach_ingredients_pots(N). 

// First goal: "activate" all of the pots by calling choose_pot_for recipe so that we can keep track of pots by adding them in the belief base
+!can_reach_ingredients_pots(N) : recipes([FirstRecipe|_]) & not all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) 
& can_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D) & cannot_reach_agent(plate,E,F) 
<-
    .print("Activating pots...");
    !choose_pot_for_recipe;
    .wait(400);
    !can_reach_ingredients_pots(N).

+!can_reach_ingredients_pots(N) : recipes([FirstRecipe|_]) & all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) 
& can_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D) & cannot_reach_agent(plate,E,F) 
<-
    .print("Waiting for a free counter...");
    .wait(500);
    !can_reach_ingredients_pots(N).

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_ingredients_pots(N)
<- 
    .print("Case: agent can only reach ingredients and pots. This plan is not applicable here -> skipping it.").



// CASE: ONLY CAN REACH PLATES + POTS ////
// For this case the agent can reach plates and pots only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Wait for the human to place an ingredient of a recipe on their shared counters.
//      2) Grab the ingredient and check if it can be placed in any of the pots. For example:
//          - we have two pots - one empty and one containing a single tomato.
//          - we have the following orders: [[onion,onion,onion],[tomato,tomato],[onion]]
//          - we pass a tomato to the agent. It matches the second recipe and the second pot.
//          - the agent places it in the pot, and starts cooking the soup
//          - if the ingredient did not match any pots and recipes, it would have been grabbed by 
//              the agent and moved to another counter (not a shared one) to free up space on the shared counters.
//      3) If a soup becomes cooked, the agent will take a dish and plate the soup and pass it to the human so that they can serve it.
// The plan will continue executing until all recipes have been completed. For the case that the human is not passing any ingredients, the agent will simply wait.
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_plates_pots : recipes([]) & can_reach_agent(plate,A,B) & can_reach_agent(pot,C,D) & cannot_reach_agent(serve,E,F)
<-
    .print("Case: agent can only reach plates and pots. All recipes have now been completed.").

// Prioritised task: if a soup is cooked, grab a plate and pass it to the human 
+!can_reach_plates_pots :  can_reach_agent(plate,Z,M)  & recipes([FirstRecipe|_])  & pot_ready(A,B) & active_pot(A,B) & can_reach_agent(pot,A,B) & object(counterTo,D,J)
& cannot_reach_agent(serve,E,F)
<-
    .print("Getting a dish and plating the soup.");
    !go_to(plate);
    !go_to(active_pot,A,B);
    !go_to(counterTo);
    remove_beliefs(pot_ready(A,B));
    .wait(400);
    !can_reach_plates_pots.

// Case 1: we passed an onion to the agent -> check if it can be placed in any pot by using the internal function. If not, move the onion away from the counter to free up space (this
// happens in the "load" plan).
+!can_reach_plates_pots : recipes(Recipes) & all_pots(used) & placed_onion(W,S) & can_reach_agent(counter,W,S)  & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots)
  &  can_reach_agent(pot,A,B) & not ( AllPots = [] ) & can_reach_agent(plate,R,T) & cannot_reach_agent(serve,E,F)
<-
    my.internal.actions.get_pot(AllPots, Recipes, onion, X, Y);
    !load(X,Y,placed_onion);
    .wait(500);
    !can_reach_plates_pots.

// Case 1: we passed a tomato to the agent -> check if it can be placed in any pot by using the internal function. If not, move the tomato away from the counter to free up space (this
// happens in the "load" plan).
+!can_reach_plates_pots : recipes(Recipes) & all_pots(used) & placed_tomato(W,S) & can_reach_agent(counter,W,S)  & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots)
  &  can_reach_agent(pot,A,B) & not ( AllPots = [] ) & can_reach_agent(plate,R,T) & cannot_reach_agent(serve,E,F)
<-
    my.internal.actions.get_pot(AllPots, Recipes, tomato, X, Y);
    !load(X,Y,placed_tomato);
    .wait(500);
    !can_reach_plates_pots.

// First goal: "activate" all of the pots by calling choose_pot_for recipe so that we can keep track of pots by adding them in the belief base
+!can_reach_plates_pots : recipes([FirstRecipe|_]) & not all_pots(used) & can_reach_agent(plate,R,T) &  can_reach_agent(pot,A,B) & cannot_reach_agent(serve,E,F)
<-
    .print("Activating pots...");
    !choose_pot_for_recipe;
    !can_reach_plates_pots.

// Waiting: the human is not passing anything on the counter and the agent has done all their current tasks -> wait until human passes stuff
+!can_reach_plates_pots : recipes([FirstRecipe|_]) & can_reach_agent(plate,R,T) &  can_reach_agent(pot,A,B) & cannot_reach_agent(serve,E,F)
<-
    .wait(500);
    .print("Waiting for human to place ingredients or plates.");
    !can_reach_plates_pots.

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_plates_pots
<-
    .print("Case: agent can only reach pots and plates. This plan is not applicable here -> skipping it.").



// CASE: ONLY CAN REACH PLATES + SERVING LOCATIONS ////
// For this case the agent can reach plates and serving locations only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Wait for the human to cook a soup. When they do, the agent will pass the human a plate on the shared counters.
//      2) Whenever the human passes back a soup on the shared counters, the agent will grab it and serve it.
//
// The plan will continue executing until all recipes have been completed. For the case that the human is not passing any soups or 
// is not cooking any for that matter, the agent will wait.
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_plates_serve : recipes([]) & can_reach_agent(plate,C,D) & can_reach_agent(serve,A,B) & cannot_reach_agent(pot,E,F)
<-
    .print("Case: agent can only reach serving locations and plates. This plan is not applicable here -> skipping it.").


// Prioritised task: if a soup is cooked, grab it and serve it
+!can_reach_plates_serve : placed_soup(K,T) & can_reach_agent(counter,K,T) & can_reach_agent(plate,C,D) & can_reach_agent(serve,A,B) & possible_counter_taken(K,T) & cannot_reach_agent(pot,E,F)
<- 
    !go_to(placed_soup);
    !go_to(serve);
    .wait(400);
    !can_reach_plates_serve.

// Pass plates: whenever a soup has been cooked, pass a dish to the human so that they can plate it
+!can_reach_plates_serve : can_reach_agent(plate,Z,M) & can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_]) & pot_ready(A,B) & pot(A,B) & object(counterTo,J,D) & object(counterTo,E,F)
& cannot_reach_agent(pot,A,B)
<-
    !go_to(plate);
    !go_to(counterTo);
    remove_beliefs(pot_ready(A,B));
    !can_reach_plates_serve.

// Waiting: if there was no free shared counters or the human is not doing anything yet, simply wait and call the plan again
+!can_reach_plates_serve : can_reach_agent(plate,Z,M) & can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_]) & cannot_reach_agent(pot,A,B)
<-
    .print("Waiting for human to cook a soup/pass one over. Or maybe all shared counters are taken.");
    .wait(500);
    !can_reach_plates_serve.

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_plates_serve
<-
    .print("Case: agent can only reach plates and serving locations. This plan is not applicable here -> skipping it.").




// CASE: ONLY CAN REACH POTS + SERVING LOCATIONS ////
// For this case the agent can reach serving locations and pots only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Wait for the human to place an ingredient of a recipe on their shared counters.
//      2) Grab the ingredient and check if it can be placed in any of the pots. For example:
//          - we have two pots - one empty and one containing a single tomato.
//          - we have the following orders: [[onion,onion,onion],[tomato,tomato],[onion]]
//          - we pass a tomato to the agent. It matches the second recipe and the second pot.
//          - the agent places it in the pot, and starts cooking the soup
//          - if the ingredient did not match any pots and recipes, it would have been grabbed by 
//              the agent and moved to another counter (not a shared one) to free up space on the shared counters.
//      3) If a soup becomes cooked, and the human has passed a dish. The agent will grab it, plate the soup and pass it to the human so that they can serve it.
// The plan will continue executing until all recipes have been completed. For the case that the human is not passing any ingredients/plates, the agent will simply wait.
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_pots_serve : recipes([]) & can_reach_agent(pot,A,B) & can_reach_agent(serve,C,D) & cannot_reach_agent(plate,E,F)
<-
    .print("Case: agent can only reach serving locations and pots. This plan is not applicable here -> skipping it.").

// Prioritised task: there is a soup ready and the human has passed a plate on the shared counters -> plate the soup and pass it to the human
+!can_reach_pots_serve : can_reach_agent(pot,A,B) & can_reach_agent(serve,R,T) & placed_dish(J,D) & can_reach_agent(counter,J,D) & cannot_reach_agent(plate,E,F)
& pot_ready(A,B)
<-
    !go_to(placed_dish);
    !go_to(active_pot,A,B);
    !go_to(serve);
    !can_reach_pots_serve.

// Case 1: we passed an onion to the agent -> check if it can be placed in any pot by using the internal function. If not, move the onion away from the counter to free up space (this
// happens in the "load" plan).
+!can_reach_pots_serve : recipes(Recipes) & all_pots(used) & placed_onion(W,S) & can_reach_agent(counter,W,S)  & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots)
  &  can_reach_agent(pot,A,B) & not ( AllPots = [] ) & can_reach_agent(serve,R,T) & cannot_reach_agent(plate,E,F)
<-
    my.internal.actions.get_pot(AllPots, Recipes, onion, X, Y);
    !load(X,Y,placed_onion);
    .wait(550);
    !can_reach_pots_serve.

// Case 2: we passed a tomato to the agent -> check if it can be placed in any pot by using the internal function. If not, move the tomato away from the counter to free up space (this
// happens in the "load" plan).
+!can_reach_pots_serve : recipes(Recipes) & all_pots(used) & placed_tomato(W,S) & can_reach_agent(counter,W,S)  & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots)
  &  can_reach_agent(pot,A,B) & not ( AllPots = [] ) & can_reach_agent(serve,R,T) & cannot_reach_agent(plate,E,F)
<-
    my.internal.actions.get_pot(AllPots, Recipes, tomato, X, Y);
    !load(X,Y,placed_tomato);
    .wait(550);
    !can_reach_pots_serve.

// First goal: "activate" all of the pots by calling choose_pot_for recipe so that we can keep track of pots by adding them in the belief base
+!can_reach_pots_serve : recipes([FirstRecipe|_]) & not all_pots(used) & can_reach_agent(serve,A,B) & can_reach_agent(pot,C,D) & cannot_reach_agent(plate,E,F)
<-
    .print("Activating pots...");
    !choose_pot_for_recipe;
    !can_reach_pots_serve.

// Waiting: the human is not passing anything on the counter and the agent has done all their current tasks -> wait until human passes stuff
+!can_reach_pots_serve : recipes([FirstRecipe|_]) & can_reach_agent(serve,A,B) & can_reach_agent(pot,C,D) & cannot_reach_agent(plate,E,F)
<-
    .print("Waiting for human to place ingredients or plates.");
    .wait(500);
    !can_reach_pots_serve.

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_pots_serve
<-
    .print("Case: agent can only reach pots and serving locations. This plan is not applicable here -> skipping it.").



// CASE: ONLY CAN REACH INGREDIENTS + SERVING LOCATIONS ////
// For this case the agent can reach plates and  ingredients only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Alternate between tomatoes and onions to:
//          -check if an ingredient can be placed in any of the pots using get_pot internal action just like in can_reach_pots_only
//          -if yes, it will psas the ingredient to the human and if not it will simply ignore it
//      2) If a soup becomes cooked, and the human has passed a dish on a shared counter, the agent plate the soup and serve it.
// The plan will continue executing untill all recipes have been completed. For the case there is no free counters to pass ingredients,
// the agent will wait for the human to pick them up.
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_ingredients_serve(N) : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y) & recipes([])
& cannot_reach_agent(pot,A,B) & cannot_reach_agent(plate,C,D)
<-
    .print("Case: agent can only reach ingredients and serving locations. All recipes have now been completed.").

// Prioritised task: if a soup is passed and the agent can reach it, grab it and then serve it
+!can_reach_ingredients_serve(N) : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_]) & placed_soup(K,L)
& can_reach_agent(counter,K,L) & cannot_reach_agent(pot,A,B) & cannot_reach_agent(plate,E,F)
<-
    !go_to(placed_soup);
    !go_to(serve);
    !can_reach_ingredients_serve(N).

// Onions: if we pass an onion can the human place it in any of the pots? if yes, pass it and if not - ignore (handled in the "pass" plan)
+!can_reach_ingredients_serve(N) : all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y) & recipes(Recipes)
& .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) & N=1
& cannot_reach_agent(pot,Q,R) & cannot_reach_agent(plate,C,D)  & possible_counter(W,S) & possible_counter(G,H) & not ((W=G) & (S=H))
<-
    my.internal.actions.get_pot(AllPots, Recipes, onion, A,B);
    !pass(A,B,onion);
    .wait(600);
    Nnew=N+1; // now try to pass a tomato
    !can_reach_ingredients_serve(Nnew).

// Tomatoes: if we pass an onion can the human place it in any of the pots? if yes, pass it and if not - ignore (handled in the "pass" plan)
+!can_reach_ingredients_serve(N) : all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y) & recipes(Recipes)
& .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) & N=2
& cannot_reach_agent(pot,Q,R) & cannot_reach_agent(plate,C,D) & possible_counter(W,S) & possible_counter(G,H) & not ((W=G) & (S=H))
<-
    my.internal.actions.get_pot(AllPots, Recipes, tomato, A,B);
    !pass(A,B,tomato);
    .wait(600);
    Nnew=N-1; // now try to pass an onion
    !can_reach_ingredients_serve(Nnew).

// First goal: "activate" all of the pots by calling choose_pot_for recipe so that we can keep track of pots by adding them in the belief base
+!can_reach_ingredients_serve(N) : recipes([FirstRecipe|_]) & not all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y)
& cannot_reach_agent(plate,A,B) & cannot_reach_agent(pot,C,D)
<-
    .print("Activating pots...");
    !choose_pot_for_recipe;
    .wait(400);
    !can_reach_ingredients_serve(N).

// Waiting: there is not free counter so we wait
+!can_reach_ingredients_serve(N) : all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y)
& cannot_reach_agent(pot,A,B) & cannot_reach_agent(plate,C,D)
<- 
    .wait(500);
    .print("Waiting for a free counter...");
    !can_reach_ingredients_serve(N).

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_ingredients_serve(N)
<- 
    .print("Case: agent can only reach ingredients and serving locations. This plan is not applicable here -> skipping it.").



//////////////////////////////// 3 OBJECTS ///////////////////////////////////////////////////
// When the agent can only reach 3 objects
// In this case, in order to succeed in making recipes, the human has to be collabarive as well
// in order to achieve a high amount of points, although the majority of the recipe depends on the agent
//
//
// CASE: ONLY CAN REACH INGREDIENTS + PLATES + SERVING LOCATIONS ////
// For this case the agent can reach plates, serving locations and ingredients. They cannot reach the pot as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Alternate between tomatoes and onions to:
//          -check if an ingredient can be placed in any of the pots using get_pot internal action just like in can_reach_pots_only
//          -if yes, it will psas the ingredient to the human and if not it will simply ignore it
//      2) If a soup becomes cooked, the agent will pass a dish to the human so they can plate the soup.
//      3) If the human passes a soup on the shared counter, the will grab it and serve it.
// The plan will continue executing untill all recipes have been completed. For the case there is no free counters to pass ingredients,
// the agent will wait for the human to pick them up.
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_ingplserve(N) : recipes([]) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & can_reach_agent(serve,V,S) & cannot_reach_agent(pot,A,B)
<-
    .print("Case: agent can only reach ingredients, plates and serving locations. All recipes have now been completed.").

// Prioritised task: if a soup is passed and the agent can reach it, grab it and then serve it
+!can_reach_ingplserve(N) : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_]) & placed_soup(K,L)
& can_reach_agent(counter,K,L) & cannot_reach_agent(pot,A,B) & can_reach_agent(plate,E,F)
<-
    !go_to(placed_soup);
    !go_to(serve);
    !can_reach_ingplserve(N).

// Secondary goal: if a soup is cooked -> pass a plate 
+!can_reach_ingplserve(N) : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & can_reach_agent(serve,V,S) & recipes([FirstRecipe|_]) 
& pot_ready(I,B) & cannot_reach_agent(pot,I,B) & possible_counter(Q,R)
<-
    !go_to(plate);
    !go_to(counterTo);
    .wait(500);
    !can_reach_ingplserve(N).

// Onions: if we pass an onion can the human place it in any of the pots? if yes, pass it and if not - ignore (handled in the "pass" plan)
+!can_reach_ingplserve(N) : all_pots(used) &  recipes(Recipes) 
& (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & can_reach_agent(serve,V,S) & cannot_reach_agent(pot,A,B)
& .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) & N=1
& possible_counter(W,S) & possible_counter(G,H) & not ((W=G) & (S=H))
<-
    my.internal.actions.get_pot(AllPots, Recipes, onion, A1,B1);
    !pass(A1,B1,onion);
    .wait(600);
    Nnew=N+1; // now try to pass a tomato
    !can_reach_ingplserve(Nnew).

// Tomatoes: if we pass an onion can the human place it in any of the pots? if yes, pass it and if not - ignore (handled in the "pass" plan)
+!can_reach_ingplserve(N) : all_pots(used) & recipes(Recipes) 
& (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & can_reach_agent(serve,V,S) & cannot_reach_agent(pot,A,B)
& .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) & N=2
& possible_counter(W,S) & possible_counter(G,H) & not ((W=G) & (S=H))
<-
    my.internal.actions.get_pot(AllPots, Recipes, tomato, A1,B1);
    !pass(A1,B1,tomato);
    .wait(600);
    Nnew=N-1; // now try to pass an onion
    !can_reach_ingplserve(Nnew).

// First goal: "activate" all of the pots by calling choose_pot_for recipe so that we can keep track of pots by adding them in the belief base
+!can_reach_ingplserve(N) : recipes([FirstRecipe|_]) & not all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y)
& can_reach_agent(plate,J,D) & cannot_reach_agent(pot,A,B)
<-
    .print("Activating pots...");
    !choose_pot_for_recipe;
    .wait(400);
    !can_reach_ingplserve(N).

// Waiting: there is not free counter so we wait
+!can_reach_ingplserve(N) : all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(serve,X,Y) & can_reach_agent(plate,J,D)
& cannot_reach_agent(pot,A,B)
<- 
    .wait(500);
    .print("Waiting for a free counter...");
    !can_reach_ingplserve(N).

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_ingplserve(N)
<-
    .print("Case: agent can only reach ingredients, plates and serving locations. This plan is not applicable here -> skipping it.").

// CASE: ONLY CAN REACH INGREDIENTS + PLATES + POTS //
// For this case the agent can reach ingredients plates and pots. They cannot reach the serving location as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Alternate between tomatoes and onions to:
//          -check if an ingredient can be placed in any of the pots using get_pot internal action just like in can_reach_pots_only
//          -if yes, it will place the ingredient into the pot and if not it will simply ignore it
//          -if that was the last ingredient for a soup, the agent will also start cooking it
//      2) If a soup becomes cooked, the agent will greab a dish, plate the soup and pass it to the human so that they can serve it.
// The plan will continue executing until all recipes have been completed. For the case that all shared counters are taken and there is no
// space for the agent to pass soups -> the agent will wait until one becomes available
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_ingplpots(N) : recipes([]) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & can_reach_agent(pot,V,S) & cannot_reach_agent(serve,A,B)
<-
    .print("Case: agent can only reach ingredients, plates and potss. All recipes have now been completed.").

// Pass a soup: a soup is ready so plate it and pass it to the human
+!can_reach_ingplpots(N) : (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(plate,X,Y) & can_reach_agent(pot,V,S) & recipes([FirstRecipe|_])
& pot_ready(V,S) & possible_counter(W,J)
<-
    !go_to(plate);
    !go_to(active_pot,V,S);
    !go_to(serve);
    !can_reach_ingplpots(N).

// Move on: if a recipe was cooked, go on to the next recipe from the orders list
+!can_reach_ingplpots(N) : increment(n) & (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & 
can_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D) & can_reach_agent(plate,E,F) 
& recipes(Recipes) & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) 
<-
    remove_beliefs(increment(n));
    Nnew = N+1;
    .wait(300);
    !can_reach_ingplpots(Nnew).

// Load ingredients: if a tomato or onion can be placed in any of the pots and be compatible with a recipe -> load it, otherwise do nothing
+!can_reach_ingplpots(N) : all_pots(used) & (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & recipes(Recipes) 
& can_reach_agent(pot,A,B) & cannot_reach_agent(serve,C,D) & can_reach_agent(plate,E,F) 
& .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) 
<-
   
    my.internal.actions.get_recipe_at_index(Recipes, N, CurrentRecipe);
    my.internal.actions.get_pot(AllPots, [CurrentRecipe], onion, A1,B1);
    !load(A1,B1,onion);
    .wait(500);
    my.internal.actions.get_pot(AllPots, [CurrentRecipe], tomato, A2,B2);
    !load(A2,B2,tomato); 
    .wait(500);
    !can_reach_ingplpots(N). 


// First goal: "activate" all of the pots by calling choose_pot_for recipe so that we can keep track of pots by adding them in the belief base
+!can_reach_ingplpots(N) : recipes([FirstRecipe|_]) & not all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(pot,X,Y)
& can_reach_agent(plate,J,D) & cannot_reach_agent(serve,A,B)
<-
    .print("Activating pots...");
    !choose_pot_for_recipe;
    .wait(400);
    !can_reach_ingplpots(N).

// Waiting: there is not free counter so we wait
+!can_reach_ingplpots(N) : all_pots(used) & (can_reach_agent(onion,Z,M) | can_reach_agent(tomato,P,T)) & can_reach_agent(pot,X,Y) & can_reach_agent(plate,J,D)
& cannot_reach_agent(serve,A,B)
<- 
    .wait(500);
    .print("Waiting for a free counter...");
    !can_reach_ingplpots(N).

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_ingplpots(N) : (Count)
<-
    .print("Case: agent can only reach ingredients, plates and pots. This plan is not applicable here -> skipping it.").


// CASE: ONLY CAN REACH INGREDIENTS + POTS + SERVING LOCATIONS ////
// For this case the agent can reach ingredients, serving locations and pots. They cannot reach the plates as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Alternate between tomatoes and onions to:
//          -check if an ingredient can be placed in any of the pots using get_pot internal action just like in can_reach_pots_only
//          -if yes, it will place the ingredient into the pot and if not it will simply ignore it
//          -if that was the last ingredient for a soup, the agent will also start cooking it
//      2) If a soup becomes cooked, and the human has passed a dish on the shared counters, the agent will grab the dish, plate the soup
//         and serve it at the serving tile/s.
// The plan will continue executing until all recipes have been completed. For the case that the human is not passing plates and all pots are full
// -> the agent will simply wait until they do
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_ingpotserve(N) : recipes([]) & can_reach_agent(pot,X,Y) & can_reach_agent(serve,Z,M) & (can_reach_agent(onion,K,L) | can_reach_agent(tomato,P,T)) 
& cannot_reach_agent(plate, A, B)
<-
    .print("Case: agent can only reach ingredients, serving locations and pots. All recipes have now been completed.").

// Soup ready: a soup has become ready and we have a dish passed by the human -> take the dish, plate the soup and serve it
+!can_reach_ingpotserve(N) : all_pots(used) & recipes(Recipes) 
& can_reach_agent(pot,X,Y) & can_reach_agent(serve,Z,M) & (can_reach_agent(onion,R,L) | can_reach_agent(tomato,P,T)) 
& placed_dish(K,J) & can_reach_agent(counter,K,J) & pot_ready(X,Y)
<-
    !go_to(placed_dish);
    !go_to(active_pot,X,Y);
    !go_to(serve);
    !can_reach_ingpotserve(N).

// Move on: if a recipe was cooked, go on to the next recipe from the orders list
+!can_reach_ingpotserve(N) : increment(n) & can_reach_agent(pot,X,Y) & can_reach_agent(serve,Z,M) & (can_reach_agent(onion,K,L) | can_reach_agent(tomato,P,T)) 
& cannot_reach_agent(plate, A, B) & recipes(Recipes) & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) 
<-
    remove_beliefs(increment(n));
    Nnew = N+1;
    .wait(300);
    !can_reach_ingpotserve(Nnew).

// Load ingredients: if a tomato or onion can be placed in any of the pots and be compatible with a recipe -> load it, otherwise do nothing
+!can_reach_ingpotserve(N) : all_pots(used) & recipes(Recipes) 
& can_reach_agent(pot,X,Y) & can_reach_agent(serve,Z,M) & (can_reach_agent(onion,K,L) | can_reach_agent(tomato,P,T)) 
& cannot_reach_agent(plate, A, B) & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) 
<-
   
    my.internal.actions.get_recipe_at_index(Recipes, N, CurrentRecipe);
    my.internal.actions.get_pot(AllPots, [CurrentRecipe], onion, A1,B1);
    !load(A1,B1,onion);
    .wait(500);
    my.internal.actions.get_pot(AllPots, [CurrentRecipe], tomato, A2,B2);
    !load(A2,B2,tomato); 
    .wait(500);
    !can_reach_ingpotserve(N). 


// First goal: "activate" all of the pots by calling choose_pot_for recipe so that we can keep track of pots by adding them in the belief base
+!can_reach_ingpotserve(N) : recipes([FirstRecipe|_]) & not all_pots(used) & can_reach_agent(pot,X,Y) & can_reach_agent(serve,Z,M) & (can_reach_agent(onion,K,L) | can_reach_agent(tomato,P,T)) 
& cannot_reach_agent(plate, A, B) 
<-
    .print("Activating pots...");
    !choose_pot_for_recipe;
    .wait(400);
    !can_reach_ingpotserve(N).    

// Waiting: there is no plates passed
+!can_reach_ingpotserve(N) : all_pots(used) & can_reach_agent(pot,X,Y) & can_reach_agent(serve,Z,M) & (can_reach_agent(onion,K,L) | can_reach_agent(tomato,P,T)) 
& cannot_reach_agent(plate, A, B) 
<- 
    .wait(500);
    .print("Waiting for plates...");
    !can_reach_ingpotserve(N).

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_ingpotserve(N)
<-
    .print("Case: agent can only reach ingredients, serving locations and pots. This plan is not applicable here -> skipping it.").


// CASE: ONLY CAN REACH SERVING LOCATIONS + PLATES + POTS ////
// For this case the agent can reach pots, serving locations and plates. They cannot reach the ingredients as 
// counters are blocking their way of getting there. In this situation the agent will:
//      1) Wait for the human to place an ingredient of a recipe on their shared counters.
//      2) Grab the ingredient and check if it can be placed in any of the pots. For example:
//          - we have two pots - one empty and one containing a single tomato.
//          - we have the following orders: [[onion,onion,onion],[tomato,tomato],[onion]]
//          - we pass a tomato to the agent. It matches the second recipe and the second pot.
//          - the agent places it in the pot, and starts cooking the soup
//          - if the ingredient did not match any pots and recipes, it would have been grabbed by 
//              the agent and moved to another counter (not a shared one) to free up space on the shared counters.
//      3) If a soup becomes cooked, the agent will grab a dish, plate the soup
//         and serve it at the serving tile/s.
// The plan will continue executing until all recipes have been completed. For the case that the human is not passing the correct ingredients ->
// the agent will pick them up and move them away from the shared counters to free up space. Later on, if those ingredients are needed the agent will reuse them
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_serveplpots : recipes([]) & can_reach_agent(serve,X,Y) & can_reach_agent(plate,Z,M) & can_reach_agent(pot,K,L) &
(cannot_reach_agent(onion,A,B) | cannot_reach_agent(tomato,C,D))
<-
    .print("Case: agent can only reach plates, serving locations and pots. All recipes have now been completed.").

// Priority task: if a soup becomes cooked -> grab a dish, plate it and serve it
+!can_reach_serveplpots : recipes([FirstRecipe|_]) & can_reach_agent(serve,X,Y) & can_reach_agent(plate,Z,M) & can_reach_agent(pot,K,L) &
(cannot_reach_agent(onion,A,B) | cannot_reach_agent(tomato,C,D)) & pot_ready(K,L) 
<-
    !go_to(plate);
    !go_to(active_pot,K,L);
    !go_to(serve);
    .wait(400);
    !can_reach_serveplpots.

// Case 1: we passed an onion to the agent -> check if it can be placed in any pot by using the internal function. If not, move the onion away from the counter to free up space (this
// happens in the "load" plan).
+!can_reach_serveplpots : recipes(Recipes) & all_pots(used) & placed_onion(W,S) & can_reach_agent(counter,W,S)  & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots)
 & not ( AllPots = [] ) & can_reach_agent(serve,X,Y) & can_reach_agent(plate,Z,M) & can_reach_agent(pot,K,L) &
(cannot_reach_agent(onion,A,B) | cannot_reach_agent(tomato,C,D)) & can_reach_human(counter,W,S)
<-
    my.internal.actions.get_pot(AllPots, Recipes, onion, F,E);
    !load(F,E,placed_onion);
    .wait(500);
    !can_reach_serveplpots.

// Case 2: we passed a tomato to the agent -> check if it can be placed in any pot by using the internal function. If not, move the tomato away from the counter to free up space (this
// happens in the "load" plan).
+!can_reach_serveplpots : recipes(Recipes) & all_pots(used) & placed_tomato(W,S) & can_reach_agent(counter,W,S)  & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots)
& not ( AllPots = [] ) & can_reach_agent(serve,X,Y) & can_reach_agent(plate,Z,M) & can_reach_agent(pot,K,L) &
(cannot_reach_agent(onion,A,B) | cannot_reach_agent(tomato,C,D)) & can_reach_human(counter,W,S)
<-
    my.internal.actions.get_pot(AllPots, Recipes, tomato, F, E);
    !load(F,E,placed_tomato);
    .wait(500);
    !can_reach_serveplpots.

// First goal: "activate" all of the pots by calling choose_pot_for recipe so that we can keep track of pots by adding them in the belief base
+!can_reach_serveplpots : recipes([FirstRecipe|_]) & not all_pots(used) & can_reach_agent(serve,X,Y) & can_reach_agent(plate,Z,M) & can_reach_agent(pot,K,L) &
(cannot_reach_agent(onion,A,B) | cannot_reach_agent(tomato,C,D))
<-
    .print("Activating pots...");
    !choose_pot_for_recipe;
    .wait(400);
    !can_reach_serveplpots.    

// Waiting: there is no plates passed
+!can_reach_serveplpots : all_pots(used) & can_reach_agent(serve,X,Y) & can_reach_agent(plate,Z,M) & can_reach_agent(pot,K,L) &
(cannot_reach_agent(onion,A,B) | cannot_reach_agent(tomato,C,D))
<- 
    .wait(500);
    .print("Waiting for ingredients...");
    !can_reach_serveplpots.

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_serveplpots
<-
    .print("Case: agent can only reach serving locations, plates and pots. This plan is not applicable here -> skipping it.").


//////////////////////////////// 4 OBJECTS ///////////////////////////////////////////////////
// When the agent can only reach 4 objects
// In this case, in order to succeed in making recipes, the human has to be collabarive as well
// in order to achieve a high amount of points, although the majority of the recipe depends on the agent
//
//
// CASE: CAN REACH EVERYTHING ////

+!can_reach_allfour(N) : all_pots(used) & can_reach_all & can_reach_all_human & recipes([])
<-  
    thought("Well done!");
    .print("Case: agent can only reach plates, serving locations and pots. All recipes have now been completed.").
 
+!can_reach_allfour(N) : all_pots(used) & can_reach_all & can_reach_all_human & recipes(Recipes) & route_cost_human(plate,PlateHuman) & route_cost(plate,PlateAgent) &
PlateHuman < PlateAgent & pot_ready(A,B)
<-
    .print("Human is closer");
    thought("You go for a plate");
    .wait(1000);
    !serve_if_no_human;
    !can_reach_allfour(N).

+!can_reach_allfour(N) : all_pots(used) & can_reach_all & can_reach_all_human & recipes(Recipes) & route_cost_human(plate,PlateHuman) & route_cost(plate,PlateAgent) &
PlateHuman > PlateAgent & pot_ready(A,B)
<-
    thought("im going to serve");
    !go_to(plate);
    !go_to(active_pot,A,B);
    !go_to(serve);
    !can_reach_allfour(N).


+!can_reach_allfour(N) : all_pots(used) & can_reach_all & can_reach_all_human & recipes(Recipes)  & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) 
 & not (pot_ready(_,_)) & N = 1
<-
   thought("I'm going for onion");
    my.internal.actions.get_pot(AllPots, Recipes, onion, A1,B1);
    !load(A1,B1,onion);
    .wait(300);
    Nnew=N+1;
    !can_reach_allfour(Nnew).

+!can_reach_allfour(N) : all_pots(used) & can_reach_all & can_reach_all_human & recipes(Recipes)  & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots) & not ( AllPots = [] ) 
& N = 2
<-
   thought("im going for tomato");
    my.internal.actions.get_pot(AllPots, Recipes, tomato, A2,B2);
    !load(A2,B2,tomato);
    .wait(300);
    Nnew=N-1;
    !can_reach_allfour(Nnew).

+!can_reach_allfour(N) : not ( all_pots(used) ) & can_reach_all & can_reach_all_human & recipes(Recipes)
<-
    
    .print("Activating pots...");
    !choose_pot_for_recipe;
    !can_reach_allfour(N).

+!can_reach_allfour(N) :  all_pots(used)  & can_reach_all & can_reach_all_human & recipes(Recipes) & not (tomato(Z,M))
<-
   
    .print("Waiting...",N);
    .wait(600);
    !can_reach_allfour(1).

+!can_reach_allfour(N) :  all_pots(used)  & can_reach_all & can_reach_all_human & recipes(Recipes)
<-
    thought("waiting for my soup to cook");
    .print("Waiting...",N);
    .wait(600);
    !can_reach_allfour(N).

+!can_reach_allfour(N)
<-
    .print("Case: agent can only reach everything. This plan is not applicable here -> skipping it.").

+!serve_if_no_human : all_pots(used) & can_reach_all & can_reach_all_human & recipes(Recipes) &  pot_ready(A,B)
<-
    thought("well now I'm going for a plate");
    !go_to(plate);
    !go_to(active_pot,A,B);
    !go_to(serve).

+!serve_if_no_human : all_pots(used) & can_reach_all & can_reach_all_human & recipes(Recipes) &  not(pot_ready(A,B))
<-
    .print("human already served").

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


+!go_to(Object,Z,M) :  agent_position(X,Y) & object(Object,Z,M) & adjacent(X,Y,Z,M) & agent_holds(dish) & not (pot_ready(Z,M)) & not (pot_cooking(Z,M))
& object(possible_counter_togo,A,B)
<- 
    .print("The agent is gonna dispose of a plate",A,B);
    compute_path(A,B);
    .wait(1000);
    !turn_to(possible_counter_togo,A,B).

+!go_to(Object,Z,M) :  agent_position(X,Y) & object(Object,Z,M) & adjacent(X,Y,Z,M) & agent_holds(onion) &  ((pot_ready(Z,M)) | (pot_cooking(Z,M)))
& object(possible_counter_togo,A,B)
<- 
    .print("The agent is gonna dispose of a plate",A,B);
    compute_path(A,B);
    .wait(1000);
    !turn_to(possible_counter_togo,A,B).

+!go_to(Object,Z,M) :  agent_position(X,Y) & object(Object,Z,M) & adjacent(X,Y,Z,M)
<- 
    .print("Go to helper function: the agent has reached the desired destination.",Z,M);
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
+!go_to(Object,Z,M) : agent_position(X,Y) & agent_holds(dish) & not (pot_ready(Z,M)) & not (pot_cooking(Z,M)) & object(possible_counter_togo, A,B)
<-
    .print("The agent is disposing of a plate:", A,B);
    compute_path(A,B);
    .wait(500);
    !turn_to(possible_counter_togo,A,B).

+!go_to(Object,Z,M) : agent_position(X,Y) & agent_holds(onion) &  ((pot_ready(Z,M)) | (pot_cooking(Z,M))) & object(possible_counter_togo, A,B)
<-
    .print("The agent is disposing of a plate:", A,B);
    compute_path(A,B);
    .wait(500);
    !turn_to(possible_counter_togo,A,B).

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
+!turn_to(Object,Z,M) : agent_position(X,Y) & object(Object, Z, M) & (X < Z)
<- 
    .print(Object,X,Y,Z,M);
    action("right");
    .wait(300);
    !execute.

+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (X < Z)
<- 
    .print(Object,X,Y,Z,M);
    action("right");
    .wait(300);
    !execute.

// Up: the agent's Y coordinate is greater than the object's Y coordinate which means the object is above the agent 
+!turn_to(Object,Z,M) : agent_position(X,Y) & object(Object, Z, M) & (Y > M) 
<- 
    action("up");
    .wait(300);
    !execute.

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

+!turn_to(Object,Z,M) : agent_position(X,Y) & object(Object, Z, M) & (Y < M)
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

+!turn_to(Object,Z,M) : agent_position(X,Y) & object(Object, Z, M) & (X > Z)
<- 
    action("left");
    .wait(300);
    !execute.

 // Fail condition: if the plan has executed too quickly and failed, the plan will be called again until successful computation
+!turn_to(Object)
<-
    .print("Turning to: ", Object," failed. Trying again...");
    !turn_to(Object).

+!turn_to(Object,Z,M)
<-
    .print("Turning to: ", Object," failed. Trying again...");
    !turn_to(Object,Z,M).
//////////////////////////////// EXECUTE ///////////////////////////////////////////////////
// This helper function is used for more clarity in the execution of other plans. It simply
// calls the external action "space" which is responsible for interaction with objects.

// Only case: this function will always execute as there is no preconditions needed for interactions as long as we have reached 
// the desired location in the previous plans.
+!execute : true
<-
    action("space").

////////////// LOAD /////////////
// This helper function is called after calling the get_pot internal function. Its purpose is to handle a single ingredient:
//      - if get_pot had returned a belief pot_to_cook, that means that this pot is ready to be cooked as the ingredient we are
//        loading in is the last one of a recipe. Therefore, after its been loaded, the agent will also start cooking the pot.
//      - if the function had returned X and Y smaller than 999, it means we can simply load the ingredient into the pot with coordinates X and Y
//      - if the function had returned X and Y equal to 999, that means that the ingredient is not compatible with any pot. Therefore, the agent
//        will pick it up and move it away from the shared counters to free up space.
//
// Ready to cook: adding this ingredient (Thing) will complete a recipe, therefore call the execute plan to start cooking the pot after loading it up
+!load(X,Y,Thing) : pot_to_cook(X,Y) & object(Thing,A,B) 
<-
    add_beliefs(increment(n));
    !go_to(Thing);
    thought("Now going to the pot");
    !go_to(active_pot,X,Y);
    thought("Time to cook");
    !execute;
    compute_path(A,B);
    .wait(300);
    remove_beliefs(pot_to_cook(X,Y)).

// Loading case: this ingredient was not the last one in a recipe so simply pick it up and place it in the pot
+!load(X,Y,Thing) : X < 999 & Y < 999 & active_pot(X,Y) & can_reach_agent(Thing,A,B)
<-
    compute_path(A,B);
    !turn_to(Thing);
    .wait(300);
    !go_to(active_pot,X,Y);
    compute_path(A,B).

// The soup is ready
+!load(X,Y,Thing) : X = 999 & Y = 999 & can_reach_agent(Thing,M,N) & pot_contents(A,B,onion,3) & object(Thing,C,D)
<-
    .print("The soup is ready -> start cooking");
    !go_to(active_pot,A,B);
    !execute;
    compute_path(C,D).

// Does not match: the ingredient is not compatible with any pots, and we can also reach it, so just ignore it
+!load(X,Y,Thing) : X = 999 & Y = 999 & can_reach_agent(Thing,M,N) 
<-
    .print("This ingredient is not compatible with any pot").



// Fail condition: the coordinates returned from get_pot do not match any active pots
+!load(X,Y,Thing)
<-
    .print("Coordinates (",X,", ",Y,") do not match any pots.").

////////////// PASS /////////////
// This helper function is called after calling the get_pot internal function. Its purpose is to handle a single ingredient and pass it to the human:
//      - if the function had returned X and Y smaller than 999, it means that the ingredient is valid and we can simply grab it and pass it to the human
//      - if there is no free counters to pass the ingredient, the agent will wait for one to be freed.
//      - if the function had returned X and Y equal to 999, that means that the ingredient is not compatible with any pot. In this case, the agent will
//        not do anything - it will ignore the ingredient.
//
// Can pass: we  have a valid pot, ingredient is compatible and a counter to pass the ingredient, so we grab it and pass it to the human
+!pass(X,Y,Thing) : X < 999 & Y < 999 & active_pot(X,Y) & object(counterTo,Z,M) 
<-
    !go_to(Thing);

    .wait(300);
    !go_to(counterTo).

// No free counter: we have a valid po, ingredient is compatible but there is no free counters to pass it -> wait and then try again
+!pass(X,Y,Thing) : X < 999 & Y < 999 & active_pot(X,Y) & not (object(counterTo,Z,M) )
<-
    .print("no free counters to pass stuff");
    .wait(500);
    !pass(X,Y,Thing).

// Does not match: the ingredient is not compatible with any pots, therefore just ignore it
+!pass(X,Y,Thing) : X = 999 & Y = 999
<-
    .print("This ingredient is not compatible with any pot").

// Fail condition: the coordinates returned from get_pot do not match any active pots
+!pass(X,Y,Thing)
<-
    .print("Coordinates (",X,", ",Y,") do not match any pots.").




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
+!choose_pot_for_recipe : pot(X,Y)  & not (active_pot(X,Y))
<-
    .print("only one pot -> using it");
    add_beliefs(active_pot(X,Y)).

+!choose_pot_for_recipe : pot(X,Y) 
<-
    add_beliefs(all_pots(used));
    .print("all pots currently in use").


+!choose_pot_for_recipe
<-
    .print("plan failed to pick a pot ").



+!discard_soup(Z,M) : pot_ready(Z,M)
<-
    !go_to(active_pot,Z,M);
    !go_to(serve).
    
+!discard_soup(Z,M) : not (pot_ready(Z,M)) & not (pot_cooking(Z,M))
<-
    !go_to(active_pot,Z,M);
    !go_to(plate);
    !discard_soup(Z,M).

+!discard_soup(Z,M) : pot_cooking(Z,M)
<-
    .print("Waiting for pot to finish cooking");
    .wait(600);
    !discard_soup(Z,M).


+!discard_soup(Z,M) 
<-
    .print("An error ocurred while discarding soup").



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

+!retrieve_current_recipe_test(N) : recipes(Recipes)
<- 
   my.internal.actions.get_recipe_at_index(Recipes, N, CurrentRecipe);
   ?countInRecipe(onion, CurrentRecipe, OnionCount);
   ?countInRecipe(tomato, CurrentRecipe, TomatoCount);
   .print("Current recipe at index ", N, " is: ", CurrentRecipe);
   .print("This recipe contains ", OnionCount, " onions and ",TomatoCount, " tomatoes.").
+!retrieve_current_recipe_test(N)
<-
    .print("Retrieving current recipes test fails").

// Test for checking the get_pot internal action
// For this test to work, you (the human) must pick up an onion/tomato and place it on a shared with the agent counter.
// The test will:
//      - "activate" all of the pots
//      - call get_pot.java and print out which pot this ingredient will be placed into.
//      - each action is delibarately slowed down significantly so that you have time to trace the outputs in the MAS console
// If X and Y return (999,999) -> the ingredient does cannot be placed in any pots because they are either full or do not match the recipe
// If X and Y return any other number: this is the coordinates of the pot that needs loaded
+!get_pots_test(Count) :  can_reach_agent(pot,X,Y) & recipes([])
<-
    .print("Test for get_pots.java. All recipes have now been completed.").

+!get_pots_test(Count) :  active_pot(X,Y) & placed_dish(K,T) & pot_ready(X,Y) & object(counterTo,S,J) &  can_reach_agent(pot,X,Y)
<- 
    .print("Pot (", X, ", ",Y,") is cooked. Going for a plate now.");
    !pick_from_human(placed_dish,1,0,0,0);
    .wait(2000);
    .print("Now going to pot(",X,", ",Y,") to pick up the soup");
    !go_to(active_pot,X,Y);
    .wait(2000);
    .print("Going to pass the the soup to the human now.");
    !go_to(counterTo);
    .wait(400);
    !get_pots_test(Count).

+!get_pots_test(Count) : recipes(Recipes) & all_pots(used) & placed_onion(W,S) & not object(counterTo,W,S)  & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots)
  & not (AllPots = [] )
<-
    .print("Cheking if a single onion fits any of those recipes: ",Recipes,". Current available pots: ",AllPots);
    my.internal.actions.get_pot(AllPots, Recipes, onion, X, Y);
    .wait(3000);
    .print("The function returned coordinates X: ",X," and Y: ",Y);
    !load(X,Y,placed_onion);
    .wait(3000);
    !get_pots_test(Count).

+!get_pots_test(Count) : recipes(Recipes) & all_pots(used) & placed_tomato(W,S) & not object(counterTo,W,S)  & .findall(active_pot(Xp, Yp), active_pot(Xp, Yp), AllPots)
  & not (AllPots = [] )
<-
    .print("Cheking if a single tomato fits any of those recipes: ",Recipes,". Current available pots: ",AllPots);
    my.internal.actions.get_pot(AllPots, Recipes, tomato, X, Y);
    .wait(3000);
    .print("The function returned coordinates X: ",X," and Y: ",Y);
    !load(X,Y,placed_tomato);
    .wait(3000);
    !get_pots_test(Count).

+!get_pots_test(Count) : recipes([FirstRecipe|_]) & not all_pots(used)
<-
    .print("Activating pots...");
    !choose_pot_for_recipe;
    !get_pots_test(Count).

+!get_pots_test(Count) : recipes([FirstRecipe|_])
<-
    .wait(500);
    .print("Waiting for human to place ingredients or plates.");
    !get_pots_test(Count).

+!get_pots_test(Count)
<-
    .print("Testing get_pots.java fails").
