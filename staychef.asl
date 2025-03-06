+agent_position(X,Y) : true
<-
    !start_now(agent).

+!start_now(agent) : true
<-
    !move(SOMEWHERE).

@move[atomic]
+!move(SOMEWHERE) : true 
<-
    .move(UP);
    .move(RIGHT);
    .move(SPACE);
    .move(LEFT);
    .move(UP);
    .move(SPACE);
    .move(RIGHT);
    .move(SPACE);
    .move(LEFT);
    .move(UP);
    .move(SPACE);
   .move(RIGHT);
   .move(SPACE);
   .move(LEFT);
    .move(UP);
    .move(SPACE);
   .move(SPACE);
    .move(LEFT);
    .move(DOWN);
    .move(SPACE);
    .wait(6500);
    .move(UP);
    .move(RIGHT);
    .move(UP);
    .move(SPACE);
    .move(RIGHT);
    .move(DOWN);
    .move(SPACE);
     .wait(500).

