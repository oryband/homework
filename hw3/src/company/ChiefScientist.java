//package company;

import java.io.*;
import java.util.*;


public class ChiefScientist implements Observer {

    private ArrayList<HeadOfLaboratory> laboratories;
    private ArrayList<Experiment> experiments;
    private Statistics statistics;
    private ScienceStore store;
    private Repository repository;
    private ChiefScientistAssistant chiefAssistant;
    private Object _lockScanAndUpdate;


    public ChiefScientist(
            ArrayList<HeadOfLaboratory> laboratories,
            ArrayList<Experiment> experiments,
            Statistics statistics,
            ScienceStore store,
            Repository repository) {

        // TODO check if need to do deep copy!!!
        this.laboratories = new ArrayList<HeadOfLaboratory>(laboratories);
        this.experiments = new ArrayList<Experiment>(experiments);
        this.statistics = statistics;
        this.store = store;
        this.repository = repository;

        // ChiefAssistant singleton factory.
        // TODO Finish singleton.
        //this.chiefAssistant = chiefAssistant.getInstance(
                //this.experiments, this);

        this.chiefAssistant = new ChiefScientistAssistant(
                this.experiments, this);
    }


    public void simulate() {
        Thread t = new Thread(this.chiefAssistant);
        t.start();
    }


    /**
     * @param specialization Required laboratory specialization.
     *
     * @return Laboratory of requested specialization, or null there aren't any.
     */
    public HeadOfLaboratory getAvailableLaboratory(String specialization) {

        for (HeadOfLaboratory headOfLaboratory : this.laboratories) {
            if (headOfLaboratory.getSpecialization().equals(specialization)) {
                return headOfLaboratory;
            }
        }

        return null;
    }


    /**
     * an experiments inform the chief that he is done.
     * chief need to update the database for rerun the ChiefAssistant by notify 
     * him (wake him up from wait())
     */
    public synchronized void update(Observable o, Object arg) {

        if (arg instanceof String) {
            String finishedExperiment = (String) arg;

            deletePreExperiments(finishedExperiment);
            changeStatusToComplete(finishedExperiment);
            updateStatisticsFinishedExperiment(finishedExperiment); 
            this.chiefAssistant.increaseNumberOfFinishedExperiments();
        } else {
            throw new RuntimeException(
                    "ERROR: Problem with casting in Chief:update()");
        }

        // Tell assistant to run available, ready experiments.
        synchronized (this.chiefAssistant) {
            this.chiefAssistant.notifyAll();
        }
    }


    public void addLaboratory(HeadOfLaboratory laboratory) {
        this.laboratories.add(laboratory);
    }


    public Repository getRepository() {
        return this.repository;
    } 

    public Statistics getStatistics() {
        return this.statistics;
    }

    public ArrayList<HeadOfLaboratory> getLaboratories() {
        return this.laboratories;
    }

    public ScienceStore getStore() {
        return this.store;
    }

    public Object getLockObject() {
        return this._lockScanAndUpdate;
    }


    // Delete pre required experiments from experiment list by the name of
    // the specific experiment that finised!!!
    public void deletePreExperiments(String expId) {

        Iterator<Experiment> experimentItr = this.experiments.iterator();
        ListIterator preRequiredExpItr = null; 

        // Iterate all Experiments
        while (experimentItr.hasNext()) {

            Experiment experimentIt = experimentItr.next();

            if (experimentIt.getExperimentId().equals(expId) == false) { 

                ArrayList<Integer> preRequiredExperiments = experimentIt
                    .getRequiredExperiments();

                // get listIterator to iterate backwards
                preRequiredExpItr = preRequiredExperiments.
                    listIterator(preRequiredExperiments.size());

                //Iterate in each experiment the pre required experiment list
                while (preRequiredExpItr.hasPrevious()) {

                    Integer i = new Integer(expId);
                    if (((Integer)preRequiredExpItr.previous()).intValue() == i.intValue()) {

                        // delete from pre required experiments
                        preRequiredExpItr.remove(); // hopefully with no special problems
                    }
                }
            } else {} // we reached the same experiment that sent here!
        }
    }

    // Changing status of experiment to COMPLETE!
    public void changeStatusToComplete(String finishedExperiment) {

        Iterator<Experiment> it = this.experiments.iterator();

        boolean found = false;
        while (it.hasNext() && !found) {
            Experiment experiment = it.next();
            if (experiment.getExperimentId().
                    equals(finishedExperiment) == true) {

                experiment.setExperimentStatus(finishedExperiment);
                found = true;
             }
        }
    }
    // Updating statistics with the finished experiment!
    public void updateStatisticsFinishedExperiment(String experimentId) {

        Iterator<Experiment> it = this.experiments.iterator();

        boolean found = false;
        while(it.hasNext() && !found) {
            Experiment expIt = it.next();

            if (expIt.getExperimentId().equals(experimentId) == true) {

                this.statistics.increaseFinishedExperiment(expIt);
                found = true;
            }
        }
    }

    // Shuting Down all Labs Gracefully my lord!
    public void shutdownAllLabs() {
        
        Iterator<HeadOfLaboratory> it = this.laboratories.iterator();
        while (it.hasNext()) {
            it.next().shutdownLab();
        }
    }

    public String toString() {


        StringBuilder result = new StringBuilder();

        String NEW_LINE = System.getProperty("line.separator");

        result.append("______________________________________" + NEW_LINE);
        result.append("           ---ChiefScientist---: " + NEW_LINE);
        result.append("Laboratories: " + NEW_LINE + this.laboratories.toString() + NEW_LINE);

        // Printed in assistant.
        //result.append("Experiments: " + NEW_LINE + this.experiments.toString() + NEW_LINE);

        result.append("Statistics: " + this.statistics.toString() + NEW_LINE);
        result.append("Store: " + this.store.toString() + NEW_LINE);
        result.append("Repository: " + this.repository.toString() + NEW_LINE);
        result.append("ChiefAssistant " + this.chiefAssistant.toString() + NEW_LINE);

        return result.toString();
    }
}
