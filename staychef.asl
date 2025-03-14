delta(X, Y, D) :- X >= Y & D = X - Y.
delta(X, Y, D) :- X < Y & D = Y - X.

manhattan_distance(X, Y, Z, M, D) :- delta(X, Z, D1) & delta(Y, M, D2) & D = D1 + D2.

free_tile(Z, M) :-  terrain(Z,M,Term) & .term2string(Term, StringTerm) & StringTerm == "e".
free_of_human(X,Y) :- not human_position(X,Y).
adjacent(X, Y, Z, M) :- (X = Z & ((Y=M+1) | (Y=M-1))) | (Y=M & ((X = Z + 1) | (X = Z - 1))).


// Calculate which object is closest and use it - e.g. onions, pots, plates, serving spots
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



!make_soup.

+!make_soup :  begin(now)
<-
    .print("Attempt to make soup");
    Recipes = [[onion,onion],[onion]];
   
    for ( .member(X,Recipes) ) {
          !collect(X);
          !go_to(plate);
          !go_to(pot);
          !go_to(serve);
       }.
 
    
    

+!make_soup : not begin(now)
<-  
    !make_soup.


+!collect([])
<- 
    !execute;
    .print("All ingredients collected for the recipe").

+!collect([Ingredient|Rest])
<- 
    .print("Collecting " , Ingredient);
    !go_to(Ingredient);
    !go_to(pot);
    !collect(Rest).


//////////// Go to  ////////////////
+!go_to(Object) :  agent_position(X,Y) & object(Object,Z,M) & adjacent(X,Y,Z,M)
<- 
    .print("arrived");
    !turn_to(Object).

+!go_to(Object) : agent_position(X,Y) & object(Object,Z,M) & (Y>M) & free_tile(X,Y-1) & not adjacent(X,Y,Z,M) &  free_of_human(X,Y-1) 
<-
    action("up");
    .wait(1200);
    !go_to(Object).

+!go_to(Object) : agent_position(X,Y) & object(Object,Z,M) & (Y<M) & free_tile(X,Y+1) & not adjacent(X,Y,Z,M) &  free_of_human(X,Y+1) 
<-
    action("down");
    .wait(1200);
    !go_to(Object).

+!go_to(Object) : agent_position(X,Y) & object(Object,Z,M) & (X>Z) & free_tile(X-1,Y) & not adjacent(X,Y,Z,M) &  free_of_human(X-1,Y) 
<-
    action("left");
    .wait(1200);
    !go_to(Object).

+!go_to(Object) : agent_position(X,Y) & object(Object,Z,M) & (X<Z) & free_tile(X+1,Y) & not adjacent(X,Y,Z,M) & free_of_human(X+1,Y) 
<-
    action("right");
    .wait(1200);
    !go_to(Object).

+!go_to(Object) : agent_position(X,Y) & (not (free_of_human(X+1,Y)) | not (free_of_human(X-1,Y)) | not (free_of_human(X,Y+1)) | not (free_of_human(X,Y-1)))
<-
    .wait(5000);
    !go_to(Object).
////////////// Turn to
+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (Y > M) 
<- 
    action("up");
    !execute;
    .wait(700).

+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (Y < M)
<- 
    action("down");
    !execute;
    .wait(700).

+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (X > Z)
<- 
    action("left");
    !execute;
    .wait(700).

+!turn_to(Object) : agent_position(X,Y) & object(Object, Z, M) & (X < Z)
<- 
    action("right");
    !execute;
    .wait(700).

+!execute : true
<-
    action("space").