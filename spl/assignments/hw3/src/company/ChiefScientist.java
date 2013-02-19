/**
 * Chief Scientist, responsible for the company's life cycle.
 *
 * @author Eldar Damari, Ory Band
 */

package company;

import java.util.Observer;
import java.util.Observable;
import java.util.ListIterator;
import java.util.ArrayList;


public class ChiefScientist implements Observer {

    private ArrayList<HeadOfLaboratory> laboratories;
    private ArrayList<Experiment> experiments;

    // final pointers.
    private final Statistics statistics;
    private final ScienceStore store;
    private final Repository repository;
    private final ChiefScientistAssistant assitant;


    public ChiefScientist(
            ArrayList<HeadOfLaboratory> laboratories,
            ArrayList<Experiment> experiments,
            Statistics statistics,
            ScienceStore store,
            Repository repository) {

        this.laboratories = laboratories;
        this.experiments = experiments;
        this.statistics = statistics;
        this.store = store;
        this.repository = repository;

        // Assitant singleton.
        this.assitant = ChiefScientistAssistant.INSTANCE;
        this.assitant.initChiefScientistAssistant(
                this.experiments, this);
    }


    /** Executes company life cycle and experiments. */
    public void simulateCompany() {
        new Thread(this.assitant).start();
    }


    /**
     * @param specialization Required laboratory specialization.
     *
     * @return Laboratory of requested specialization,
     * or null if there aren't any.
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
     * Wakes up (from wait()) chief assitant and executes new experiments.
     */
    public synchronized void update(Observable o, Object arg) {

        if (arg instanceof String) {
            String finishedExperiment = (String) arg;

            removedPrerequiredExperiments(finishedExperiment);
            changeStatusToComplete(finishedExperiment);
            updateStatisticsFinishedExperiment(finishedExperiment); 
            this.assitant.incrementNumberOfFinishedExperiments();
        } else {
            throw new RuntimeException(
                    "ChiefScientist.update() - Bad argument, failed casting.");
        }

        // Tell assistant to run available, ready experiments.
        synchronized (this.assitant) {
            this.assitant.notifyAll();
        }
    }


    /**
     * Adds a laboratory to the company.
     *
     * @param laboratory lab to add.
     */
    public void addLaboratory(HeadOfLaboratory laboratory) {
        this.laboratories.add(laboratory);
    }


    // Getters
    public Repository getRepository() {
        return this.repository;
    } 

    public Statistics getStatistics() {
        return this.statistics;
    }

    public ArrayList<HeadOfLaboratory> getLaboratories() {
        return this.laboratories;
    }

    public ScienceStore getScienceStore() {
        return this.store;
    }


    /**
     * Removes a completed experiment from all dependant experiments.
     *
     * @param completedExperiment completed experiment id.
     */
    public void removedPrerequiredExperiments(String completedExperiment) {

        for (Experiment experiment : this.experiments) {

            if ( ! experiment.getId().equals(completedExperiment) ) { 

                ArrayList<Integer> requiredExperiments =
                    experiment.getRequiredExperiments();


                
                // Remove completed experiment from all dependant experiments.
                ListIterator<Integer> it = requiredExperiments.listIterator();
                while (it.hasNext()) {
                    Integer experimentId = new Integer(completedExperiment);
                    if ((it.next()).intValue() == experimentId.intValue()) {
                        it.remove();
                    }
                }
            }
        }
    }


    /**
     * Changes experiment status to 'complete'.
     *
     * @param finishedExperiment experiment id.
     */
    public void changeStatusToComplete(String finishedExperiment) {

        for (Experiment e : this.experiments) {
            if (e.getId().equals(finishedExperiment)) {
                e.setStatus("COMPLETE");
                return;
             }
        }
    }


    /**
     * Updates statistics that an experiment has been completed.
     *
     * @param experimentId completed experiment id.
     */
    public void updateStatisticsFinishedExperiment(String experimentId) {

        for (Experiment e : this.experiments) {
            if (e.getId().equals(experimentId)) {
                this.statistics.increaseFinishedExperiment(e);
                return;
            }
        }
    }


    /** Graceful lab shutdown. */
    public void shutdownAllLabs() {
        for (HeadOfLaboratory lab : this.laboratories) {
            lab.shutdownLab();
        }
    }


    public String toString() {

        StringBuilder result = new StringBuilder();
        String N = System.getProperty("line.separator");

        result.append(N);
        result.append("Chief Scientist:" + N);
        result.append(this.laboratories.toString() + N);
        result.append(this.store.toString() + N);
        result.append(this.repository.toString() + N);
        result.append(this.assitant.toString() + N);
        result.append(this.statistics.toString() + N);

        return result.toString();
    }
}
