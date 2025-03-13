free_tile(Z, M) :-  terrain(Z,M,Term) & .term2string(Term, StringTerm) & StringTerm == "e".
adjacent(X, Y, Z, M) :- (X = Z & ((Y=M+1) | (Y=M-1))) | (Y=M & ((X = Z + 1) | (X = Z - 1))).


!make_soup.

+!make_soup :  agent_position(3,2)
<-
    .print("Attempt to make soup");
    .wait(2000);
    !collect_onions(3);
    action("space");
    !go_to(plate);
    action("space");
    !go_to(pot);
    action("space");
    !go_to(serve);
    action("space").
    

+!make_soup : not agent_position(3,2)
<-  
    !make_soup.


+!collect_onions(Onions_left) : Onions_left>0
<-
    .print("collecting an onion");
    !go_to(onion);
    action("space");
    !go_to(pot);
    action("space");
    !collect_onions(Onions_left - 1).
     

+!collect_onions(0)
<-
    .print("Collected an onion for the recipe").

//////////// Go to Onion ////////////////
+!go_to(onion) :  agent_position(X,Y) & onion(Z,M) & adjacent(X,Y,Z,M)
<- 
    .print("arrived");
    !turn_to_onion.

+!go_to(onion) : agent_position(X,Y) & onion(Z,M) & (Y>M) & free_tile(X,Y-1) & not adjacent(X,Y,Z,M)
<-
    action("up");
    .wait(1200);
    !go_to(onion).

+!go_to(onion) : agent_position(X,Y) & onion(Z,M) & (Y<M) & free_tile(X,Y+1) & not adjacent(X,Y,Z,M)
<-
    action("down");
    .wait(1200);
    !go_to(onion).

+!go_to(onion) : agent_position(X,Y) & onion(Z,M) & (X>Z) & free_tile(X-1,Y) & not adjacent(X,Y,Z,M)
<-
    action("left");
    .wait(1200);
    !go_to(onion).

+!go_to(onion) : agent_position(X,Y) & onion(Z,M) & (X<Z) & free_tile(X+1,Y) & not adjacent(X,Y,Z,M)
<-
    action("right");
    .wait(1200);
    !go_to(onion).

/////////// turn to onion ///////////
+!turn_to_onion : agent_position(X,Y) & onion(Z,M) & (Y > M)
<- 
    action("up");
    .wait(700).


+!turn_to_onion : agent_position(X,Y) & onion(Z,M) & (Y < M)
<- 
    action("down");
    .wait(700).


+!turn_to_onion : agent_position(X,Y) & onion(Z,M) & (X > Z)
<- 
    action("left");
    .wait(700).


+!turn_to_onion : agent_position(X,Y) & onion(Z,M) & (X < Z)
<- 
    action("right");
    .wait(700).

//////////// Go to pot ////////////////
+!go_to(pot) :  agent_position(X,Y) & pot(Z,M) & adjacent(X,Y,Z,M)
<- 
    .print("arrived");
    !turn_to_pot.

+!go_to(pot) : agent_position(X,Y) & pot(Z,M) & (Y>M) & free_tile(X,Y-1) & not adjacent(X,Y,Z,M)
<-
    action("up");
    .wait(1200);
    !go_to(pot).

+!go_to(pot) : agent_position(X,Y) & pot(Z,M) & (Y<M) & free_tile(X,Y+1) & not adjacent(X,Y,Z,M)
<-
    action("down");
    .wait(1200);
    !go_to(pot).

+!go_to(pot) : agent_position(X,Y) & pot(Z,M) & (X>Z) & free_tile(X-1,Y) & not adjacent(X,Y,Z,M)
<-
    action("left");
    .wait(1200);
    !go_to(pot).

+!go_to(pot) : agent_position(X,Y) & pot(Z,M) & (X<Z) & free_tile(X+1,Y) & not adjacent(X,Y,Z,M)
<-
    action("right");
    .wait(1200);
    !go_to(pot).

/////////// turn to pot ///////////
+!turn_to_pot : agent_position(X,Y) & pot(Z,M) & (Y > M)
<- 
    action("up");
    .wait(700).


+!turn_to_pot : agent_position(X,Y) & pot(Z,M) & (Y < M)
<- 
    action("down");
    .wait(700).


+!turn_to_pot : agent_position(X,Y) & pot(Z,M) & (X > Z)
<- 
    action("left");
    .wait(700).


+!turn_to_pot : agent_position(X,Y) & pot(Z,M) & (X < Z)
<- 
    action("right");
    .wait(700).



//////////// Go to plate ////////////////
+!go_to(plate) :  agent_position(X,Y) & plate(Z,M) & adjacent(X,Y,Z,M)
<- 
    .print("arrived");
    !turn_to_plate.

+!go_to(plate) : agent_position(X,Y) & plate(Z,M) & (Y>M) & free_tile(X,Y-1) & not adjacent(X,Y,Z,M)
<-
    action("up");
    .wait(1200);
    !go_to(plate).

+!go_to(plate) : agent_position(X,Y) & plate(Z,M) & (Y<M) & free_tile(X,Y+1) & not adjacent(X,Y,Z,M)
<-
    action("down");
    .wait(1200);
    !go_to(plate).

+!go_to(plate) : agent_position(X,Y) & plate(Z,M) & (X>Z) & free_tile(X-1,Y) & not adjacent(X,Y,Z,M)
<-
    action("left");
    .wait(1200);
    !go_to(plate).

+!go_to(plate) : agent_position(X,Y) & plate(Z,M) & (X<Z) & free_tile(X+1,Y) & not adjacent(X,Y,Z,M)
<-
    action("right");
    .wait(1200);
    !go_to(plate).

/////////// turn to plate ///////////
+!turn_to_plate : agent_position(X,Y) & plate(Z,M) & (Y > M)
<- 
    action("up");
    .wait(700).


+!turn_to_plate : agent_position(X,Y) & plate(Z,M) & (Y < M)
<- 
    action("down");
    .wait(700).


+!turn_to_plate : agent_position(X,Y) & plate(Z,M) & (X > Z)
<- 
    action("left");
    .wait(700).


+!turn_to_plate : agent_position(X,Y) & plate(Z,M) & (X < Z)
<- 
    action("right");
    .wait(700).


//////////// Go to serve ////////////////
+!go_to(serve) :  agent_position(X,Y) & serve(Z,M) & adjacent(X,Y,Z,M)
<- 
    .print("arrived");
    !turn_to_serve.

+!go_to(serve) : agent_position(X,Y) & serve(Z,M) & (Y>M) & free_tile(X,Y-1) & not adjacent(X,Y,Z,M)
<-
    action("up");
    .wait(1200);
    !go_to(serve).

+!go_to(serve) : agent_position(X,Y) & serve(Z,M) & (Y<M) & free_tile(X,Y+1) & not adjacent(X,Y,Z,M)
<-
    action("down");
    .wait(1200);
    !go_to(serve).

+!go_to(serve) : agent_position(X,Y) & serve(Z,M) & (X>Z) & free_tile(X-1,Y) & not adjacent(X,Y,Z,M)
<-
    action("left");
    .wait(1200);
    !go_to(serve).

+!go_to(serve) : agent_position(X,Y) & serve(Z,M) & (X<Z) & free_tile(X+1,Y) & not adjacent(X,Y,Z,M)
<-
    action("right");
    .wait(1200);
    !go_to(serve).

/////////// turn to serve ///////////
+!turn_to_serve : agent_position(X,Y) & serve(Z,M) & (Y > M)
<- 
    action("up");
    .wait(700).


+!turn_to_serve : agent_position(X,Y) & serve(Z,M) & (Y < M)
<- 
    action("down");
    .wait(700).


+!turn_to_serve : agent_position(X,Y) & serve(Z,M) & (X > Z)
<- 
    action("left");
    .wait(700).


+!turn_to_serve : agent_position(X,Y) & serve(Z,M) & (X < Z)
<- 
    action("right");
    .wait(700).




