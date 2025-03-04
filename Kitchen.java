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

                    // Start a background thread that repeatedly sends STAY
                    stayThread = new Thread(() -> {
                        try {
                            while (active_game) {
                                try {
                                    // Emit "action": { "action": "STAY" } - overcooked server handles the game mechanics of it
                                    JSONObject actionData = new JSONObject().put("action", "STAY");
                                    socket.emit("action", actionData);
                                } catch (JSONException e) {
                                    e.printStackTrace();
                                }

                                Thread.sleep(500); // every 0.5s
                            }
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    });
                    stayThread.start();
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

            // Connect to the server
            socket.connect();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public boolean executeAction(String agName, Structure act) {
        // for now do nothing
        System.out.println("executeAction => " + agName + " does " + act);
        return true;
    }
}
