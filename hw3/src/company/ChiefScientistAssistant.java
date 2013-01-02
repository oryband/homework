//package company;

import java.util.Map;
import java.io.*;
import java.util.*;


public enum ChiefScientistAssistant implements Runnable {

    INSTANCE;

    private ArrayList<RunnableExperiment> runnableExperiments = null;
    private int completedExperiments = 0;
    private ChiefScientist chiefScientist = null;


    public void initChiefScientistAssistant(
            ArrayList<Experiment> experiments, ChiefScientist chiefScientist) {

        if (experiments == null) {
            throw new RuntimeException(
                    "Cannot re-initialize runnableExperiments");
        } else {
            this.runnableExperiments = new ArrayList<RunnableExperiment>();

            for (Experiment e : experiments) {
                this.runnableExperiments.add(
                        new RunnableExperiment(e, chiefScientist));
            }
        }

        if (chiefScientist == null) {
            throw new RuntimeException("Cannot re-initialize chiefAssistant.");
        } else {
            this.chiefScientist = chiefScientist;
        }
    }


    public synchronized void run() {

        while (this.completedExperiments != this.runnableExperiments.size()) {

            // Find incomplete experiments to execute.
            for (RunnableExperiment runnableExperiment : this.runnableExperiments) {

                Experiment experiment = runnableExperiment.getExperiment();

                // If current experiment is incomplete,
                // and if there are no prerequired experiments for it.
                if ( experiment.getStatus().equals("INCOMPLETE")
                        && experiment.getRequiredExperiments().size() == 0 ) {

                    String specialization = experiment.getSpecialization();

                    // Get laboratory for requested specialization for
                    // current experiment.
                    HeadOfLaboratory headOfLaboratory =
                        this.chiefScientist.getAvailableLaboratory(specialization);

                    // Execute experiment if there's a lab,
                    // or purhcase one and then execute experiment.
                    if (headOfLaboratory == null) {
                        this.chiefScientist.getStore().purchaseLaboratory(
                                this.chiefScientist,
                                this.chiefScientist.getStatistics(),
                                specialization); 

                        headOfLaboratory = this.chiefScientist.getAvailableLaboratory(specialization);
                    }

                    executeExperiment(
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

        this.chiefScientist.shutdownAllLabs();

        // Print statistics.
        System.out.println(this.chiefScientist.getStatistics().toString());
    }


    /**
     * Purchases missing equipment for experiment and executes it.
     *
     * @param headOfLaboratory lab where the experiment will take place in.
     * @param runnableExperiment experiment to execute.
     */
    public void executeExperiment(
            HeadOfLaboratory headOfLaboratory,
            RunnableExperiment runnableExperiment) {
 
        Experiment experiment = runnableExperiment.getExperiment();

        HashMap<String, Integer> shoppingList =
            generateShoppingList(experiment.getRequiredEquipment());


        if (shoppingList.size() > 0) {
            // Purchase items in shopping list.
            this.chiefScientist.getStore().purchaseEquipmentPackages(
                    this.chiefScientist.getRepository(),
                    this.chiefScientist.getStatistics(),
                    shoppingList);
        }


        experiment.setExperimentStatus("INPROGRESS");
        headOfLaboratory.executeExperiment(runnableExperiment);
    }
    

    /**
     * Generates equipment hash map for items missing in repo, for a certain
     * experiment.
     *
     * @param equipment Equipment necessary for experiment.
     *
     * @return equipment hash map with missing items from repository, which
     * should be purchased from the science store.
     */
    public HashMap<String, Integer> generateShoppingList(
            HashMap<String, Integer> equipment) {

        HashMap<String,Integer> repository =
            this.chiefScientist.getRepository().getRepository(); 

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

        for (RunnableExperiment e : this.runnableExperiments) {
            result.append(e.toString() + NEW_LINE);
        }

        return result.toString();
    }
}
