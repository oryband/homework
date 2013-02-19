/**
 * Represents a laboratory.
 *
 * @author Eldar Damari, Ory Band.
 */

package company;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


public class HeadOfLaboratory {

    private final String name;
    private final String specialization;
    private int numberOfScientists;
    private ExecutorService executor;


    public HeadOfLaboratory(
            String headLabName,
            String specialization,
            int numberOfScientists) {

        this.name = new String(headLabName);
        this.specialization = new String(specialization);
        this.numberOfScientists = numberOfScientists;
        this.executor = Executors.newFixedThreadPool(numberOfScientists);
    }
    

    /**
     * Executes an experiment.
     *
     * @param experiment experiment to execute.
     */
    public void executeExperiment(Runnable experiment) {
        this.executor.execute(experiment);
    }

    /**
     * Adds scientists to the laboratory.
     *
     * @param amount amount of scientists to add.
     */
    public void addScientists(int amount) {
        this.numberOfScientists += amount;
    }


    public String getSpecialization() {
        return this.specialization;
    }


    /** Graceful shutdown. */
    public void shutdownLab() {
        this.executor.shutdown();
    }


    public String toString() {

        StringBuilder result = new StringBuilder();
        String N = System.getProperty("line.separator");
        
        result.append(N);
        result.append("Head Of Laboratory: ");
        result.append(this.name + ", ");
        result.append(this.specialization);
        result.append(this.numberOfScientists + " scientists." + N);

        return result.toString();
    }
}
