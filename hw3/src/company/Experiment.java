//package company;

import java.util.Comparator;
import java.io.*;
import java.util.*;


public class Experiment {

    private final String id;
    private final String specialization;
    private final HashMap<String, Integer> requiredEquipment;
    private final int reward;

    private ArrayList<Integer> prerequirementsExperiments;
    private int currentRunTime;
    private final int requiredRunTime;
    private String status;


    public Experiment(
            String id,
            ArrayList<Integer> preExperiments,
            String specialization,
            HashMap<String,Integer> requiredEquipment,
            int runtime,
            int reward,
            String status) {

        this.id = id;
        this.specialization = specialization;
        this.prerequirementsExperiments = preExperiments;
        this.requiredEquipment = requiredEquipment;
        this.reward = reward;
        this.currentRunTime = runtime;
        this.requiredRunTime = runtime;
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
    public void setRealRunTime(int runtime){
        this.currentRunTime = runtime;
    }

    public void setStatus(String status) {
        this.status = status;
    }



    public String toString(){

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");

        result.append("______________________________________" + NEW_LINE);
        result.append("Experiment id: " + id + NEW_LINE);
        result.append("Specialization: " + specialization + NEW_LINE);
        result.append("PrerequirementsExperiments: " +
                prerequirementsExperiments.toString() + NEW_LINE);

        result.append("Required Equipment: " + this.requiredEquipment.toString() + NEW_LINE);
        result.append("Run Time: " + requiredRunTime + NEW_LINE);
        result.append("Reward: " + reward + NEW_LINE);
        result.append("Status: " + status + NEW_LINE);
        result.append("______________________________________" + NEW_LINE);
        return result.toString();
    }
}
