//package company;

import java.io.*;
import java.util.*;


public class Experiment{

    private String id;
    private String specialization;
    private ArrayList<Integer> prerequirementsExperiments;
    private HashMap<String, Integer> requiredEquipment;
    private int reward;
    private int runTimeInHours;
    private String status;


    public Experiment(String id,
            ArrayList<Integer> preExperiments,
            String specialization,
            HashMap<String,Integer> requiredEquipment,
            int runtime,
            int reward,
            String status){

        this.id = id;
        this.specialization = specialization;
        this.prerequirementsExperiments = new ArrayList<Integer>();
        this.prerequirementsExperiments = preExperiments;
        this.requiredEquipment = new HashMap<String,Integer>();
        this.requiredEquipment = requiredEquipment;
        this.reward = reward;
        this.runTimeInHours = runtime;
        this.status = status;
    }

    // Getters
    public String getExperimentId(){
        return this.id;
    }
    public String getExperimentSpecialization(){
        return this.specialization;
    }
    public ArrayList<Integer> getExperimentPreRequirementsExperiments(){
        return this.prerequirementsExperiments;
    }
    public HashMap<String,Integer> getExperimentRequiredEquipments(){
        return this.requiredEquipment;
    }
    public int getExperimentRunTime(){
        return this.runTimeInHours;
    }
    public int getExperimentReward(){
        return this.reward;
    }
    public String getExperimentStatus(){
        return this.status;
    }

    // Setters
    public void setExperimentRunTime(int runtime){
        this.runTimeInHours = runtime;
    }
    public void setExperimentStatus(String status) {
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

        Iterator<EquipmentPackage> it = requiredEquipment.iterator();
        while (it.hasNext()) {

            result.append(it.next().toString() + NEW_LINE);
        }
        result.append("Run Time: " + runTimeInHours + NEW_LINE);
        result.append("Reward: " + reward + NEW_LINE);
        result.append("Status: " + status + NEW_LINE);
        result.append("______________________________________" + NEW_LINE);
        return result.toString();
    }
}
