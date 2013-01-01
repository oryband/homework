//package company;

import java.io.*;
import java.util.*;


public class Statistics {

    private int budget;
    private int rewards;
    private int moneySpent;

    private ArrayList<Scientist> scientistsPurchased;
    private ArrayList<EquipmentPackage> equipmentPackagesPurchased;
    private ArrayList<Laboratory> laboratoriesPurchased;
    private ArrayList<Experiment> experiments;

    public Statistics(int budget) {
        this.budget = budget;
        this.rewards = 0 ;
        this.moneySpent = 0;
        this.scientistsPurchased = new ArrayList<Scientist>();
        this.equipmentPackagesPurchased = new ArrayList<EquipmentPackage>();
        this.laboratoriesPurchased = new ArrayList<Laboratory>();
        this.experiments = new ArrayList<Experiment>(); 
    }

    public Statistics() {
        this(0);
    }

    // Setters
    // TODO Remove this, and set budget in constructor (used in util).
    public void setBudget(int i) {
        this.budget = i;
    }
    public void increaseFinishedExperiment(Experiment experiment) {
        this.experiments.add(experiment);
    }


    public void addPurchasedEquipment(EquipmentPackage equipmentPackage) {
        this.budget -= equipmentPackage.getPrice();
        this.moneySpent += equipmentPackage.getPrice();
        this.equipmentPackagesPurchased.add(equipmentPackage);
    }

    public void addPurchasedScientist(Scientist scientist) {
        this.budget -= scientist.getPrice();
        this.moneySpent += scientist.getPrice();
        this.scientistsPurchased.add(scientist);
    }

    public void addPurchasedLaboratory(Laboratory laboratory) {
        this.budget -= laboratory.getPrice();
        this.moneySpent += laboratory.getPrice();
        this.laboratoriesPurchased.add(laboratory);
    }


    public void addReward(int reward) {
        this.budget += reward;
        this.rewards += reward;
    }

    public int getBudget() {
        return this.budget;
    }

    public String toString() {

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");

        result.append("______________________________________" + NEW_LINE);
        result.append("           ---SATISTICS---: " + NEW_LINE);
        result.append("Budget: " + this.budget + NEW_LINE);
        result.append("Reward: " + this.rewards + NEW_LINE);
        result.append("Money Spent: " + this.moneySpent + NEW_LINE);
        result.append("Finished Experiments: " + 
                this.experiments.toString() + NEW_LINE);
        result.append("Equipment Packages Purchased : " + 
                this.equipmentPackagesPurchased.toString() + NEW_LINE);
        result.append("Laboratoris Purchased: " + 
                this.laboratoriesPurchased.toString() + NEW_LINE);
        result.append("Scientists Purchased: " + 
                this.scientistsPurchased.toString() + NEW_LINE);

        return result.toString();
    }
}
