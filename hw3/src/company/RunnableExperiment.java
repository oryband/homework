//package company;

import java.io.*;
import java.util.*;


public class RunnableExperiment extends Observable implements Runnable {

    private Experiment experiment;
    private long experimentRealRunTime;
    private ChiefScientist chief;


    public RunnableExperiment(
            Experiment experiment, ChiefScientist chiefScientist) {

        this.experiment = experiment;
        addObserver(chiefScientist);
        this.experimentRealRunTime = 0;
        this.chief = chiefScientist;
    }


    public void run() {

        System.out.println("Start: " + this.experiment.getExperimentId());

        // Experiment still in progress
        while (experiment.getExperimentRealRunTime() > 0) {

            this.experimentRealRunTime += new Date().getTime();

            Repository repo = this.chief.getRepository();

            repo.aquireEquipment(
                    this.experiment.getRequiredEquipment(), this.experiment); 

            // Sleep 8 hours (1 hour = 100 miliseconds).
            try {
                Thread.sleep(800);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            // Return equipment taken for the day.
            this.chief.getRepository().releaseEquipment(
                    this.experiment.getRequiredEquipment());

            if (this.experiment.getExperimentRealRunTime() <= 8) {

                this.experiment.setExperimentRealRunTime(0);

                this.experimentRealRunTime = 
                    new Date().getTime() - this.experimentRealRunTime;

                // Reward calculation.
                if (this.experimentRealRunTime <= 
                        (this.experiment.getExperimentRunTime() * 115)) {

                    // 115% of reard gain.
                    this.chief.getStatistics().addReward(this.experiment.getExperimentReward());

                } else {  // 10% of reward gained.
                    this.chief.getStatistics().addReward((this.experiment.getExperimentReward()) * 10);
                }

                System.out.println("Experiment End : " + this.experiment.getExperimentId());
                System.out.println("Experiment End with run time:  " + this.experimentRealRunTime); 
        
                // Notify observers (ChiefScientist) that experiment is complete.
                setChanged();
                notifyObservers(this.experiment.getExperimentId());
            } else {
                this.experiment.setExperimentRealRunTime(
                        this.experiment.getExperimentRealRunTime() - 8);

                this.experimentRealRunTime -= new Date().getTime();

                // Sleep for 16 hours ~ 16,000 miliseconds
                try {
                    Thread.sleep(1600);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    // Getters
    public Experiment getExperiment() {
        return this.experiment;
    }


    public String toString() {

        StringBuilder result = new StringBuilder();

        String NEW_LINE = System.getProperty("line.separator");

        result.append("______________________________________" + NEW_LINE);
        result.append("         ---Runnable Experiment---:" + NEW_LINE);

        result.append("Experiment: " + NEW_LINE + this.experiment.toString() + NEW_LINE);
        result.append("Real Run Time: " + this.experimentRealRunTime + NEW_LINE);

        return result.toString();
    }
}
