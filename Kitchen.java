import jason.environment.Environment;
import jason.asSyntax.Structure;
import jason.asSyntax.Literal;
import io.socket.client.IO;
import io.socket.client.Socket;
import io.socket.emitter.Emitter;

public class Kitchen extends Environment {
    int[] staychefLocation = {1, 1};
    int[] humanLocation = {2, 3};
    private Socket socket;

    @Override
    public void init(String[] args) {
        // Set initial percepts
        addPercept(Literal.parseLiteral("location(staychef, pos(" + staychefLocation[0] + "," + staychefLocation[1] + "))"));
        addPercept(Literal.parseLiteral("location(human, pos(" + humanLocation[0] + "," + humanLocation[1] + "))"));

        // Attempt to connect to the Overcooked demo server
        try {
            // If you see Overcooked at http://localhost in your browser, this is correct.
            // If you see :5000 in your browser, do "http://localhost:5000"
            socket = IO.socket("http://localhost");

            socket.on(Socket.EVENT_CONNECT, new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    System.out.println("Connected to Overcooked demo server!");

                    // Emit a custom event so the server can broadcast that the Java agent is here
                    socket.emit("java_connected", "Hello from Java agent!");
                }
            });

            socket.on("human_action", new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    if (args.length > 0) {
                        System.out.println("Java agent received 'human_action' with data: " + args[0]);
                        // If needed, parse the data as JSON, e.g. cast to JSONObject or similar
                        // Then you can do your own symbolic reasoning or react in your agent code
                    } else {
                        System.out.println("Java agent received 'human_action' with NO data");
                    }
                }
            });
            socket.on("paramm", new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    if (args.length > 0) {
                        System.out.println("Java agent received 'paramm' with data: " + args[0]);
                        // If needed, parse the data as JSON, e.g. cast to JSONObject or similar
                        // Then you can do your own symbolic reasoning or react in your agent code
                    } else {
                        System.out.println("Java agent received 'human_action' with NO data");
                    }
                }
            });
            
            // Optionally handle disconnects or errors
            socket.on(Socket.EVENT_DISCONNECT, new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    System.out.println("Java agent disconnected from Overcooked server.");
                }
            });

            socket.connect();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public boolean executeAction(String ag, Structure act) {
        System.out.println("Agent " + ag + " is doing " + act);

        if (act.getFunctor().equals("stay")) {
            // For demonstration, update a percept
            removePercept(Literal.parseLiteral("location(human, pos(" + humanLocation[0] + "," + humanLocation[1] + "))"));
            humanLocation = new int[]{1, 3};
            addPercept(Literal.parseLiteral("location(human, pos(" + humanLocation[0] + "," + humanLocation[1] + "))"));
        }

        return true;
    }
}
