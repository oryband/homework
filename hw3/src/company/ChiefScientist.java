package company;

import stat.*;
import utilities.*;

import java.io.*;
import java.util.*;

public class ChiefScientist implements Observer{

    private ArrayList<HeadOfLaboratory> laboratories;
    private ArrayList<Experiment> experiments;
    private Statistics statistics;
    private ScienceStore store;
    private Repository repository;
    private ChiefScientistAssistant chiefAssistant;


    public ChiefScientist(ArrayList<HeadOfLaboratory> laboratories,
                          ArrayList<Experiment> experiments,
                          Statistics statistics,
                          ScienceStore store,
                          Repository repository,
                          ChiefScientistAssistant chiefAssistant) {

        this.laboratories = laboratories;
        this.experiments = experiments;
        this.statistics = statistics;
        this.store = store;
        this.repository = repository;
        this.chiefAssistant = chiefAssistant;
            }
    // an experiments inform the chief that he is done.
    // chief need to update the database for rerun the ChiefAssistant by notify 
    // him (wake him up from wait())
    // who returns stuff to repo?
    // who is updating status of experiment?
    public void update(){


    }


    //Getters
    public Repository getRepository() {
        return this.repository;
    } 
    public Statistics getStatistics() {
        return this.statistics;
    }
    public ArrayList<HeadOfLaboratory> getLaboratories() {
        return this.laboratories;
    }
    public String toString() {

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");

        result.append("______________________________________" + NEW_LINE);
        result.append("           ---ChiefScientist---: " + NEW_LINE);
        result.append("Laboratories: " + this.laboratories.toString() + NEW_LINE);
        result.append("Experiments: " + this.experiments.toString() + NEW_LINE);
        result.append("Statistics: " + this.statistics.toString() + NEW_LINE);
        result.append("Store: " + this.store.toString() + NEW_LINE);
        result.append("Repository: " + this.repository.toString() + NEW_LINE);
        result.append("ChiefAssistant " + this.chiefAssistant.toString() + NEW_LINE);
        return result.toString();
    }


}
