// Helper function calculating manhattan distance
delta(X, Y, D) :- X >= Y & D = X - Y.
delta(X, Y, D) :- X < Y & D = Y - X.
manhattan_distance(X, Y, Z, M, D) :- delta(X, Z, D1) & delta(Y, M, D2) & D = D1 + D2.



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
    plate(Z, M) &
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
    can_reach_agent(tomato,Z,M) &
    agent_position(AX, AY) &
    manhattan_distance(AX, AY, Z, M, D) &
    not (tomato(Z1, M1) &
         manhattan_distance(AX, AY, Z1, M1, D1) &
         D1 < D).

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

possible_counter_taken(Z,M) :-
    can_reach_agent(counter,Z,M) &
    can_reach_human(counter,Z,M) & 
    placed_stuff(Z,M).


closest_counter(Z,M) :-
    possible_counter(Z,M) &
    human_position(Hx, Hy) &
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
    can_reach_agent(onion,I,S)  &
    can_reach_agent(tomato,V,F).

!start.


+!start : begin(now)
<- 

    ?recipes(Recipes);
    .print("Retrieved recipes: " , Recipes);
    ?count_objects(pot(_,_), Count);
    //action("down");
    .wait(5000);
    .wait(300);
    //!collect_recipe(Count);
    //!can_reach_ingredients_pots(Count);
    //!can_reach_ingredients_plates;
    //!can_reach_ingredients_serve.
    //!can_reach_plates_pots(Count).
    //!can_reach_plates_serve.
    !can_reach_allfour(Count);
    !can_reach_ingpotserve(Count);
    !can_reach_serveplpots(Count);
    !can_reach_ingplpots(Count);
    !can_reach_pots_serve(Count);

    //!can_reach_ingredients_only;
    //!can_reach_plates_only;
    //!can_reach_serve_only;
    //!can_reach_pots_only(Count).
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
// 1) Check for the current recipe how many ingredients it needs.
// 2) Pass each of them indiviually to the humans on counters shared by both the human and the agent. If there is no
//    free counters because the human is stalling, the agent will wait until one frees.
// 3) This plan continues until all of the recipes from the Orders list have been served.
// 4) For the case where a human is not cooperative/makes a mistake and does a wrong order, the agent will simply repeat the 
//    same order by passing the needed ingredients all over again.
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_ingredients_only : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & recipes([])
<-
    .print("Case: agent can only reach ingredients. All recipes have now been completed.").

// Recursive case: There is still orders to be made. In this case for each order we pass the ingredients to the human and
// do a call to the plan again so that we can proceed with the rest of the recipes.
+!can_reach_ingredients_only : (can_reach_agent(onion,Z,M)| can_reach_agent(tomato,P,T)) & recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, OnionCount);
    ?countInRecipe(tomato, FirstRecipe, TomatoCount);
    !pass_to_human(onion,OnionCount);
    !pass_to_human(tomato,TomatoCount);
    .wait(3000); // EDIT
    !can_reach_ingredients_only.

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_ingredients_only
<-
    .print("Case: agent can only reach ingredients. This plan is not applicable here -> skipping it.").

// CASE: ONLY REACH PLATES
// For this case the agent can reach plates and plates only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
// 1) Grab a plate for each recipe in the orders list and pass it to the human on a counter
//  that is reachable by both of them.
// 2) If there is no free counters because the human is stalling, the agent will wait until one frees.
// 3) This plan continues until all of the recipes from the Orders list have been served.
// 4) For the case where a human is not cooperative/makes a mistake and does a wrong order, the agent will simply repeat the 
//    same order by passing the needed plate all over again.
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_plates_only : can_reach_agent(plate,X,Y) & recipes([])
<-
    .print("Case: agent can only reach ingredients. All recipes have now been completed.").

// Recursive case: There is still orders to be made. In this case for each order we pass the plate to the human and
// do a call to the plan again so that we can proceed with the rest of the recipes.
+!can_reach_plates_only : can_reach_agent(plate,X,Y) & recipes([FirstRecipe|_])
<-
    !pass_to_human(plate,1);
    .wait(3000); // EDIT
    !can_reach_plates_only.

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_plates_only
<-
    .print("Case: agent can only reach plates. This plan is not applicable here -> skipping it.").

// CASE: ONLY REACH SERVE SPOT
// For this case the agent can reach serving spots and serving spots only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
// 1) Wait for the human to place a soup on their shared counters.
// 2) Grab the soup and serve it.
// 3) This plan continues until all of the recipes from the Orders list have been served.
// 4) For the case where a human is not cooperative/makes a mistake and does a wrong order, the agent will simply repeat the 
//    same order by serving the soup for a reward of 0 points and waiting for a new soup all over again.
// NOTE: serving spots are also considered bins
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_serve_only : can_reach_agent(serve,X,Y) & recipes([])
<-
    .print("Case: agent can only reach serving spots. All recipes have now been completed.").

// Recursive case: There is still orders to be made. In this case for each order we pick up a soup from the human (from 
// a shared counter) and do a call to the plan again so that we can proceed with the rest of the recipes.
+!can_reach_serve_only : can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_])
<-
    !pick_from_human(placed_soup,1,0);
    !go_to(serve);
    !can_reach_serve_only.

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_serve_only
<-
    .print("Case: agent can only reach serving spots. This plan is not applicable here -> skipping it.").
////////////

// CASE: ONLY REACH POT
// For this case the agent can reach serving pots and serving pots only. They cannot reach the other objects as 
// counters are blocking their way of getting there. In this situation the agent will:
// 1) Wait for the human to place an ingredient of the recipe on their shared counters.
// 2) Grab the ingredient and place it in a pot. Repeat this until the pot has all ingredients needed for a recipe.
// 3) Wait for the human to place a plate on the shared conuters. Grab the plate, pick up the soup and hand it back to the 
//    human (on the shared counters).
// 4) This plan continues until all of the recipes from the Orders list have been served.
//
// Special cases: human places ingredients that are not of this recipe on the shared counters -> the agent will pick them up
// and place them on other counters to free up space. Consequtively, when making other recipes, they don't need to wait for
// the human to place ingredients/plates on the shared counters as they can use those ones that have been previously "incorrect".
//
//
// Base case: The orders list has been completed and therefore we can exit the plan.
+!can_reach_pots_only(Count) :  can_reach_agent(pot,X,Y) & recipes([])
<-
    .print("Case: agent can only reach pots. All recipes have now been completed.").

+!can_reach_pots_only(Count) : can_reach_agent(pot,X,Y) & recipes([FirstRecipe|_]) & active_pot(X,Y) & placed_dish(X,Y)
<- 
    !pick_from_human(placed_dish,1,0);
    .wait(2000); // EDIT 
    remove_beliefs(active_pot(X,Y));
    !pass_to_human(placed_soup,1);
    .wait(4000);
    !can_reach_pots_only(Count).

// Recursive case:
+!can_reach_pots_only(Count) : can_reach_agent(pot,X,Y) & recipes([FirstRecipe|_]) & active_pot(X,Y)
<- 
    ?countInRecipe(onion, FirstRecipe, OnionCount);
    ?countInRecipe(tomato, FirstRecipe, TomatoCount);
    .print("The agent has to pick up ",OnionCount," onions and ", TomatoCount," tomatoes.");
    !pick_from_human(placed_onion, OnionCount,1);
    !pick_from_human(placed_tomato, TomatoCount,1);
    !go_to(active_pot);
    !can_reach_pots_only(Count).

// Case:
+!can_reach_pots_only(Count) : can_reach_agent(pot,X,Y) & recipes([FirstRecipe|_]) & not(active_pot(X,Y))
<-
    !choose_pot_for_recipe(FirstRecipe,Count);
    !can_reach_pots_only(Count).

// Fail condition: this plan was not applicable, so we exit it and do not execute it.
+!can_reach_pots_only(Count)
<-
    .print("Case: agent can only reach pots. This plan is not applicable here -> skipping it.").


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
    .wait(3000); // PASS THE NEXT RECIPE
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
    !pick_load(onion,NOnion,NOnion);
    !pick_load(tomato,NTomato,NTomato);
    !execute;
    .wait(400);
    !pick_from_human(placed_dish,1,0);
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
    !pick_from_human(placed_onion,OnionCount,1);
    !pick_from_human(placed_tomato,TomatoCount,1);
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
    !pick_from_human(placed_soup,1,0);
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


+!can_reach_pots_serve(Count) : can_reach_agent(pot,Z,M) & can_reach_agent(serve,X,Y) & recipes([FirstRecipe|_]) 
<-
    ?countInRecipe(onion, FirstRecipe, OnionCount);
    ?countInRecipe(tomato, FirstRecipe, TomatoCount);
    !choose_pot_for_recipe(FirstRecipe,Count);
    !pick_from_human(placed_onion,OnionCount,1);
    !pick_from_human(placed_tomato,TomatoCount,1);
    !execute;
    !pick_from_human(placed_dish,1,0);
    .wait(2000);
    !go_to(active_pot);
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
    !pick_from_human(placed_soup,1,0);
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
    !pick_from_human(placed_soup,1,0);
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
    !pick_load(onion,OnionCoun,OnionCount);
    !pick_load(tomato,TomatoCount,TomatoCount);
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
    !pick_load(onion,OnionCount,OnionCount);
    !pick_load(tomato,TomatoCount,TomatoCount);
    !execute;
    !pick_from_human(placed_dish,1,0);
    !go_to(active_pot);
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
    !pick_from_human(placed_onion,OnionCount,1);
    !pick_from_human(placed_tomato,TomatoCount,1);
    !execute;
    .wait(3000);
    !go_to(plate);
    !go_to(active_pot);
    !go_to(serve);
    remove_beliefs(active_pot(_,_));
    .wait(500);
    !can_reach_serveplpots(Count).

+!can_reach_serveplpots(Count)
<-
    .print("Case: agent can only reach serving locations, plates and pots. This plan is not applicable here -> skipping it.").


+!can_reach_allfour(Count) : recipes([])
<-
    .print("Case: agent can reach everything. All recipes have now been completed.").


+!can_reach_allfour(Count) : can_reach_all &  recipes([FirstRecipe|_])
<-
    ?countInRecipe(onion, FirstRecipe, OnionCount);
    ?countInRecipe(tomato, FirstRecipe, TomatoCount);
    !choose_pot_for_recipe(FirstRecipe,Count);
    .wait(400);
    !pick_load(onion,OnionCount,OnionCount);
    //!pick_load(tomato,TomatoCount,TomatoCount);
    !execute;
    .wait(6500);
    !pick_load(plate,1,1);
    !go_to(active_pot);
    remove_beliefs(active_pot(_,_));
    !go_to(serve);
    .wait(600);
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
// Base case: The agent has arrived at the desired location and therefore we can exit the plan by calling turn_to.
+!go_to(Object) :  agent_position(X,Y) & object(Object,Z,M) & adjacent(X,Y,Z,M)
<- 
    .print("Go to helper function: the agent has reached the desired destination.");
    .wait(300);
    !turn_to(Object).

// Recursive case: The agent is not at the desired location, so it calls the A* algorithm to compute a path for it
// After that it calls go_to so that the base case can be reached.
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


+!pick_load(Thing,N,NOriginal) : N = 0
<-
    .print("done picking").

+!pick_load(Think,N,NOriginal) : discard_recipe(X,Y)
<-
    !discard_soup(X,Y);
    remove_beliefs(discard_recipe(X,Y));
    !pick_load(Thing,Noriginal,Noriginal).

+!pick_load(Thing,N,NOriginal) : N>0 & active_pot(X,Y) & recipes_new([FirstRecipe|_])
<-
    remove_beliefs(recipes_new(_));
    ?countInRecipe(Thing, FirstRecipe, Number);
    .print("found ",Thing);
    !pick_load(Thing,Number,Number).

+!pick_load(Thing,N,NOriginal) : N > 0 & active_pot(X,Y) & pot(matches_recipe)
<-
    remove_beliefs(pot(matches_recipe));
    !go_to(Thing);
    !go_to(active_pot);
    .wait(400);
    Nnew = N -1 ;
    !pick_load(Thing,Nnew,NOriginal).

+!pick_load(Thing,N,NOriginal) : N > 0 & active_pot(X,Y)
<- 
     match_pot_with_recipe(X,Y);
    !pick_load(Thing,N,NOriginal).

// PICKING STUFF UP FROM HUMAN
+!pick_from_human(Thing,0,M)
<-
    .print("All things picked up from the counter").

+!pick_from_human(Thing,N,M) :  placedObject(Thing,X,Y) & N>0 & M = 0
// M = 0 means its not a pot we are going to
<-
    !go_to(Thing);
    Nnew = N - 1;
    !pick_from_human(Thing,Nnew,M).

+!pick_from_human(Thing,N,M) :  placedObject(Thing,X,Y) & N>0 
<-
    
    !go_to(Thing);
    .wait(300);
    !go_to(active_pot);
    Nnew = N - 1;
    !pick_from_human(Thing,Nnew,M).


+!pick_from_human(Thing,N,M) :  placedObject(OtherThing,X,Y) & not (placed_soup(X,Y)) & N>0 & possible_counter_taken(X,Y)
<-
    !go_to(OtherThing);
    .print("from here");
    .wait(300);
    !go_to(free_counter_agent);
    !pick_from_human(Thing,N,M).

+!pick_from_human(Thing,N,M)
<-
    .print("waiting for object to be passed from human");
    .wait(500);
    .print(Thing);
    !pick_from_human(Thing,N,M).
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
+!choose_pot_for_recipe(Recipe,Count) : pot(X,Y) & discard_recipe(X,Y)
<-
    !discard_soup(X,Y);
    remove_beliefs(discard_recipe(X,Y));
    !choose_pot_for_recipe(Recipe, Count).
// one empty pot
+!choose_pot_for_recipe(Recipe, Count) : pot(X,Y) &
  not (pot_contents(X,Y, onion, OCount)) &
  not (pot_contents(X,Y, tomato, TCount)) & Count == 1 & not (active_pot(X,Y))
<-
    .print("only one pot and it is empty -> using it");
    add_beliefs(active_pot(X,Y)).

// one pot with ingredients that match a recipe
+!choose_pot_for_recipe(Recipe,Count) : pot(X,Y) & Count == 1 & not (active_pot(X,Y))
<-
    match_pot_with_recipe(X,Y);
    !choose_pot_for_recipe(Recipe,Count).

+!choose_pot_for_recipe(Recipe,Count) : pot(X,Y) & Count == 1 & active_pot(X,Y)
<-
    .print("active pot is now selected").



+!choose_pot_for_recipe(Recipe, Count)
: Count=2 & empty_pots
  
<-
  ?object(pot,X,Y);
  add_beliefs(active_pot(X,Y)).


+!choose_pot_for_recipe(Recipe, Count)
: Count =2 & not (empty_pots)
<-
  match_pot_with_recipe(X,Y);
  !choose_pot_for_recipe(Recipe,Count).


+!choose_pot_for_recipe(Recipe, Count) : pot(X,Y) & pot_matches_recipe(X,Y,Recipe)
<-
  .print("Pot (", X, ",", Y, ") partially matches a recipe - use that one");
  add_beliefs(active_pot(X,Y)).
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
+!collect_recipe(Count) :  recipes([])
<-
    .print("Recipes done").

+!collect_recipe(Count) : recipes_new([FirstRecipe|_]) & active_pot(X,Y)
<- 
    ?countInRecipe(onion, FirstRecipe, NOnion);
    ?countInRecipe(tomato, FirstRecipe, NTomato);
    .print("comes here");
    .print("have to pick up additional ",NOnion," onions and ", NTomato," tomatoes.");
    !pick(onion, NOnion,NOnion);
    !pick(tomato, NTomato,NTomato);
    remove_beliefs(active_pot(X,Y));
    
    remove_beliefs(recipes_new(_));
    !collect_recipe(Count).

+!collect_recipe(Count) : recipes([FirstRecipe|_]) & active_pot(X,Y)
<-
    ?countInRecipe(onion, FirstRecipe, NOnion);
    ?countInRecipe(tomato, FirstRecipe, NTomato);
    .print("have to pick up additional ",NOnion," onions and ", NTomato," tomatoes.");
    !pick(onion, NOnion,NOnion);
    !pick(tomato, NTomato,NTomato);
    remove_beliefs(active_pot(X,Y));
    !collect_recipe(Count).


+!collect_recipe(Count) : recipes([FirstRecipe|_]) & not(active_pot(X,Y))
<-
    !choose_pot_for_recipe(FirstRecipe,Count);
    .print("fails after here");
    !collect_recipe(Count).

+!collect_recipe(Count)
<-
    .wait(2000);
    .print("failllll bruh");
    !collect_recipe(Count).