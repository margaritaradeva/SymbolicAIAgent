import jason.environment.Environment;
import jason.asSyntax.Literal;
import jason.asSyntax.Structure;

import io.socket.client.IO;
import io.socket.client.Socket;
import io.socket.emitter.Emitter;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONArray;

public class Kitchen extends Environment {

    private Socket socket; // Socket connection
    private volatile boolean active_game = false; // volatile so its visible to all threads, even the sleeping ones
    private Thread stayThread;

    @Override
    public void init(String[] args) {
        // "fake" percept - just testing stuff
        addPercept(Literal.parseLiteral("location(staychef, pos(1,1))"));

        try {
            // Connect to Overcooked server on the localhost
            socket = IO.socket("http://localhost"); 

            // 2) On connect => "join" a waiting game
            socket.on(Socket.EVENT_CONNECT, new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    System.out.println("Connected to Overcooked server"); // print in the MAS console for debugging
                    try {
                        // Build a JSON object for the "join" event 
                        // Generally, those parameters are already passed by the user already. But if the Overcooked server fails to create a game with the user
                        // passed parameters, it will use those ones.
                        JSONObject data_to_join = new JSONObject()
                                .put("create_if_not_found", false) // or true if you want it to create a game
                                .put("params", new JSONObject()
                                        .put("playerZero", "human")
                                        .put("playerOne", "human")
                                        .put("layout", "scenario1_s")
                                        .put("gameTime", "120")
                                        .put("dataCollection", "off")
                                        .put("layouts", new JSONArray().put("scenario1_s"))
                                )
                                .put("game_name", "overcooked");

                        // Emit "join" to Overcooked with this JSON
                        socket.emit("join", data_to_join);

                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
            });

            // Listen for "start_game" => user has pressed the start button and the game is active 
            socket.on("start_game", new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    System.out.println("Received 'start_game' => game is now active (user pressed start button)"); // for debuging purposes, printed in MAS console
                    active_game = true;
                }
            });

            // 4) Listen for "end_game" or "end_lobby" => stop the STAY loop as the user disconnected due to clicking a button
            socket.on("end_game", new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    System.out.println("Received end_game => stopping STAY thread");
                    active_game = false;
                }
            });
            socket.on("end_lobby", new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    System.out.println("Received end_lobby => stopping STAY thread");
                    active_game = false;
                }
            });

            // If the overcooked server or this server disconnects => also stop
            socket.on(Socket.EVENT_DISCONNECT, new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    System.out.println("Disconnected from Overcooked server.");
                    active_game = false;
                }
            });

            socket.on("java_layout", new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    if (args.length > 0) {
                        System.out.println("Received layout: " + args[0]);

                        try {
                            // layoutObj consists of layout name and the terrain (e.g. XXPXOX )
                            JSONObject layoutObject = new JSONObject(args[0].toString());
                            String layoutName = layoutObject.getString("layout_name"); // extract the layout
                            JSONArray terrainArray = layoutObject.getJSONArray("terrain"); // extract the terrain
                            
                            for (int row=0; row<terrainArray.length(); row++){
                                // Iterate through the terain and save the perceptions
                                JSONArray terrainRow = terrainArray.getJSONArray(row);
                                for (int column=0; column<terrainRow.length(); column++){
                                    String item = terrainRow.getString(column); //exctract each item e.g soup, tile, chef
                                    if (item.equals(" ")){
                                        item = "E";
                                    }
                                    addPercept("staychef", Literal.parseLiteral("terrain("+row+","+column+","+item+")"));
                                }
                            }

                            addPercept("staychef", Literal.parseLiteral("layout_name(" + layoutName + ")"));
                        
                        } catch (JSONException e){
                            e.printStackTrace();
                        }
                    }
                }
            });
            
            socket.on("java_state_update", new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    if (args.length > 0) {
                        // System.out.println("Received state update: " + args[0]);
                        try{
                            // The JSON from the server looks like:
                            // {
                            //   "score": what players have achieved so far by delivering orders,
                            //   "time_left": how much time there's left
                            //   "state":{
                            //       "players":[
                            //  human:  {"orientation":what way the human faces, "position":what row and column they occupy, "held_object":e.g. onion},
                            //  agent:  {"orientation","position", "held_object"}
                            //       ],
                            //       "objects": e.g. "cook_time", "is_ready"
                            //    },
                            //   "potential":null
                            // }
                            // We filter out the relevant information and pass it as perceptions
                            JSONObject message = new JSONObject(args[0].toString());

                            // First exctract the "state" and then player information
                            JSONObject state = message.getJSONObject("state");
                            // Extract the "players" field now
                            JSONArray players = state.getJSONArray("players");

                            // Remove any old beliefs about the environment
                            removePerceptsByUnif("staychef", Literal.parseLiteral("human_orientation(_,_)"));
                            removePerceptsByUnif("staychef", Literal.parseLiteral("human_position(_,_)"));
                            removePerceptsByUnif("staychef", Literal.parseLiteral("human_holds(_)"));

                            removePerceptsByUnif("staychef", Literal.parseLiteral("agent_orientation(_,_)"));
                            removePerceptsByUnif("staychef", Literal.parseLiteral("agent_position(_,_)"));
                            removePerceptsByUnif("staychef", Literal.parseLiteral("agent_holds(_)"));

                            // Loop over player 1 (human) and player 2 (agent)
                            for (int i=0; i<players.length();i++) {
                                JSONObject playersInfo = players.getJSONObject(i);

                                // orientation
                                JSONArray orientations = playersInfo.getJSONArray("orientation");
                                int xOrient = orientations.getInt(0);
                                int yOrient = orientations.getInt(1);

                                // position
                                JSONArray positions = playersInfo.getJSONArray("position");
                                int xPos = positions.getInt(0);
                                int yPos = positions.getInt(1);

                                // held_object !! CAN be null too
                                String heldObject = "none";
                                if (!playersInfo.isNull("held_object")){
                                    JSONObject heldObj = playersInfo.getJSONObject("held_object");
                                    String objName = heldObj.getString("name"); // e.g. onion
                                    heldObject = objName;
                                }

                                if (i==0){
                                    addPercept("staychef", Literal.parseLiteral("human_orientation(" + xOrient + ","+yOrient+")"));
                                    addPercept("staychef", Literal.parseLiteral("human_position(" + xPos + ","+yPos+")"));
                                    addPercept("staychef", Literal.parseLiteral("human_holds("+heldObject+")"));
                                }   
                                else if (i==1) {
                                    addPercept("staychef", Literal.parseLiteral("agent_orientation(" + xOrient + ","+yOrient+")"));
                                    addPercept("staychef", Literal.parseLiteral("agent_position(" + xPos + ","+yPos+")"));
                                    addPercept("staychef", Literal.parseLiteral("agent_holds(" + heldObject +")"));
                                }

                            }
                       
                       
                       
                        } catch (JSONException e) {
                            e.printStackTrace();
                        }
                        
                    }
                }
            });
            
            // Connect to the server
            socket.connect();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public boolean executeAction(String agName, Structure act) {
        String action = act.getTerm(0).toString();
        
            try{
                JSONObject actionData = new JSONObject().put("action", action);
                socket.emit("action",actionData);
                System.out.println("Action " + action);
            } catch (JSONException e) {
                e.printStackTrace();
            }
        
        return true;
    }

}
