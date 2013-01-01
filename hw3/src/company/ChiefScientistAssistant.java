//package company;

import java.util.Map;
import java.io.*;
import java.util.*;


public class ChiefScientistAssistant implements Runnable {

    private ArrayList<RunnableExperiment> experiments;
    private int completedExperiments;
    private final ChiefScientist chief;

    // TODO Finish singleton.
    // Singleton chief assistant.
    /*private static ChiefScientistAssistant assistant = null;

    public static synchronized ChiefScientistAssistant 
        getInstance(ArrayList<Experiment> experimentsToRun,ChiefScientist chief) {

        if (assistant == null) {

            assistant = new ChiefScientistAssistant(experimentsToRun,chief);
            return assistant;

        } else {
            return assistant;
        }
    }*/


    public ChiefScientistAssistant(
            ArrayList<Experiment> experimentsToRun, ChiefScientist chief) {

        this.experiments = new ArrayList<RunnableExperiment>();

        for (Experiment e : experimentsToRun) {
            this.experiments.add(new RunnableExperiment(e, chief));
        }

        this.completedExperiments = 0;
        this.chief = chief;
    }


    public synchronized void run() {

        while (this.completedExperiments != this.experiments.size()) {

            // Find incomplete experiments to execute.
            for (RunnableExperiment runnableExperiment : this.experiments) {

                Experiment experiment = runnableExperiment.getExperiment();

                // If current experiment is incomplete,
                // and if there are no prerequired experiments for it.
                if ( experiment.getStatus().equals("INCOMPLETE")
                        && experiment.getRequiredExperiments().size() == 0 ) {

                    String specialization = experiment.getSpecialization();

                    // Get laboratory for requested specialization for
                    // current experiment.
                    HeadOfLaboratory headOfLaboratory =
                        this.chief.getAvailableLaboratory(specialization);

                    // Execute experiment if there's a lab,
                    // or purhcase one and then execute experiment.
                    if (headOfLaboratory == null) {
                        this.chief.getStore().purchaseLaboratory(
                                this.chief,
                                this.chief.getStatistics(),
                                specialization); 

                        headOfLaboratory = this.chief.getAvailableLaboratory(specialization);
                    }

                    prepareExperimentToExecute(
                            headOfLaboratory, runnableExperiment);
                }
            }


            // Wait for another experiment to finish before starting another one.
            try {
                this.wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        this.chief.shutdownAllLabs();

        // Print statistics.
        System.out.println(this.chief.getStatistics().toString());
    }


    /**
     * preparing experiment to be execute
     * checks for equipment in repo if missing buying
     * changing status and execute.
     */
    public void prepareExperimentToExecute(
            HeadOfLaboratory lab, RunnableExperiment runnableExperiment) {
 
        Experiment experiment = runnableExperiment.getExperiment();

        HashMap<String,Integer> shoppingList =
            generateShoppingList(experiment.getRequiredEquipment());

        if (shoppingList.size() > 0) {
            // Go and purchase items in HashMap
            this.chief.getStore().purchaseEquipmentPackages(
                    this.chief.getRepository(),
                    this.chief.getStatistics(),
                    shoppingList);
        }

        experiment.setExperimentStatus("INPROGRESS");
        lab.addExperimentToExecute(runnableExperiment);
    }
    

    /**
     * Checks in repo for equipments, and if mising, fill hash map with
     * equipments to buy.
     */
    public HashMap<String, Integer> generateShoppingList(HashMap<String, Integer> equipment) {

        HashMap<String,Integer> repository =
            this.chief.getRepository().getRepository(); 

        HashMap<String,Integer> equipmentToPurchase =
            new HashMap<String,Integer>(); 


        // Generate missing equipment shopping list (hash map).
        for (Map.Entry<String, Integer> entry : equipment.entrySet()) {

            String equipmentType = entry.getKey();

            // If equipment type is in repo.
            if (repository.containsKey(equipmentType)) {

                // If there aren't enought of this equipment type.
                if ( repository.get(equipmentType).intValue() <
                        equipment.get(equipmentType).intValue() ) {

                    Integer diff = new Integer(
                            equipment.get(equipmentType).intValue() -
                            repository.get(equipmentType).intValue());

                    equipmentToPurchase.put(equipmentType, diff);
                }
            } else {  // Equipment type not in repository (at all).
                Integer amount = new Integer(equipment.get(equipmentType).intValue());
                equipmentToPurchase.put(equipmentType, amount);
            }
        }

        return equipmentToPurchase;
    }


    // Increase Number Of finishedExperiment by 1;
    public void increaseNumberOfFinishedExperiments() {
        this.completedExperiments += 1;
    }


    public String toString() {

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");

        result.append("______________________________________" + NEW_LINE);
        result.append("           ---Chief Scientist Assistant---: " + NEW_LINE);

        for (RunnableExperiment e : this.experiments) {
            result.append(e.toString() + NEW_LINE);
        }

        return result.toString();
    }
}
