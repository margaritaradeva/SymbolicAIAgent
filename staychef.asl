!start_now.

+!start_now : agent_position(3,2)
<-
    !move_up(up);
    !move_right(right);
    !move_nothing(space);
    !move_left(left);
    !move_up(up);
    !move_nothing(space);
    !move_right(right);
    !move_nothing(space);
    !move_left(left);
    !move_up(up);
    !move_nothing(space);
   !move_right(right);
   !move_nothing(space);
   !move_left(left);
    !move_up(up);
    !move_nothing(space);
   !move_nothing(space);
    !move_left(left);
    !move_down(down);
    !move_nothing(space);
    .wait(2000);
    !move_up(up);
    !move_right(right);
    !move_up(up);
    !move_nothing(space);
    !move_right(right);
    !move_down(down);
    !move_nothing(space);
    !start_now.

+!start_now : not agent_position(3,2)
<- 
    !start_now.

+!move_up(up) : true 
<-
    move_direction(UP);
    .wait(500).
    
+!move_down(down) : true 
<-
    move_direction(DOWN);
    .wait(500).
    
+!move_left(left) : true 
<-
    move_direction(LEFT);
    .wait(500).
    
+!move_right(right) : true 
<-
    move_direction(RIGHT);
    .wait(500).
    

+!move_nothing(space) : true 
<-
    move_direction(SPACE);
    .wait(500).
    

