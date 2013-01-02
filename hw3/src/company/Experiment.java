/** @author Eldar Damari, Ory Band */

package company;

import java.util.Comparator;
import java.util.HashMap;
import java.util.ArrayList;


public class Experiment {

    private final String id;
    private final String specialization;
    private final HashMap<String, Integer> requiredEquipment;
    private final int reward;

    private ArrayList<Integer> prerequirementsExperiments;
    private int currentRunTime;  // ms.
    private final int requiredRunTime;  // ms.
    private String status;


    public Experiment(
            String id,
            ArrayList<Integer> requiredExperiments,
            String specialization,
            HashMap<String,Integer> requiredEquipment,
            int runTime,
            int reward,
            String status) {

        this.id = id;
        this.specialization = specialization;
        this.prerequirementsExperiments = requiredExperiments;
        this.requiredEquipment = requiredEquipment;
        this.reward = reward;
        this.currentRunTime = runTime;
        this.requiredRunTime = runTime;
        this.status = status;
    }


    // Getters
    public String getId() {
        return this.id;
    }

    public String getSpecialization() {
        return this.specialization;
    }

    public ArrayList<Integer> getRequiredExperiments() {
        return this.prerequirementsExperiments;
    }

    public HashMap<String, Integer> getRequiredEquipment() {
        return this.requiredEquipment;
    }

    public int getRequiredRunTime() {
        return this.requiredRunTime;
    }

    public int getCurrentRunTime() {
        return this.currentRunTime;
    }

    public int getReward() {
        return this.reward;
    }

    public String getStatus() {
        return this.status;
    }


    // Setters
    public void setRealRunTime(int runtime) {
        this.currentRunTime = runtime;
    }

    public void setStatus(String status) {
        this.status = status;
    }


    public String toString() {

        StringBuilder result = new StringBuilder();
        String N = System.getProperty("line.separator");

        result.append("Experiment:" + N);
        result.append("id: " + id + N);
        result.append("Run Time: " + requiredRunTime + N);
        result.append("Reward: " + reward + N);
        result.append("Status: " + status + N);
        result.append("Specialization: " + specialization + N);
        result.append("Required Experiments: " + prerequirementsExperiments.toString() + N);
        result.append("Required Equipment: " + this.requiredEquipment.toString() + N);

        return result.toString();
    }
}
