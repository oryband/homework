public class Main {
    public static void main(String[] args) {
        // Fetch file paths.
        String registered = args[0];
        String arrivals   = args[1];
        String results    = args[2];

        // Process flight and print results to file.
        FlightManager f = new FlightManager();

        f.processFlight(registered, arrivals, results);
    }
}

