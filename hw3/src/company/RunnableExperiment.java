//package company;

import java.io.*;
import java.util.*;


public class RunnableExperiment extends Observable implements Runnable {

    private Experiment experiment;
    private long experimentRealRunTime;
    private Date date;
    private ChiefScientist chief;


    public RunnableExperiment(Experiment experiment, ChiefScientist chiefScientist) {
        this.experiment = experiment;
        addObserver(chiefScientist);
        this.experimentRealRunTime = 0;
        this.chief = chiefScientist;
    }


    public void run() {

        System.out.println("Experiment start: " + this.experiment.getExperimentId());

        // Experiment still in progress
        while (experiment.getExperimentRunTime() != 0) {

            this.date = new Date();
            this.experimentRealRunTime += date.getTime();

            Repository repo = this.chief.getRepository();
            //System.out.println(">>>" + this.chief.getRepository().toString() + " - "+this.experiment.getExperimentId());

            repo.aquireEquipment(
                    this.experiment.getRequiredEquipment(), this.experiment); 

            //System.out.println("---" + this.chief.getRepository().toString() +  " - "+this.experiment.getExperimentId());

            // Sleep 8 hours
            try {
                // Sleep 8 hours (1 hour = 100 miliseconds).
                Thread.currentThread().sleep(800);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            if (this.experiment.getExperimentRunTime() <= 8) {

                this.experiment.setExperimentRunTime(0);

                //Release equipment back to repo - if someone is buying right  now?
                //problem with returning & purchasing? - check sync carefully in repo!!

                this.chief.getRepository().releaseEquipment
                    (this.experiment.getRequiredEquipment());

                date = new Date();
                this.experimentRealRunTime = 
                    date.getTime() - this.experimentRealRunTime;

                // reward claculation and updating statistics
                if (this.experimentRealRunTime <= 
                        ((this.experiment.getExperimentRunTime() / 100.0) * 115)){
                    // reward gained.
                    this.chief.getStatistics().addReward(this.experiment.getExperimentReward());

                } else {
                    // 10% of reward gained.
                    this.chief.getStatistics().addReward((this.experiment.getExperimentReward() / 100.0) * 10);

                }
                System.out.println("Experiment End : " + this.experiment.getExperimentId());
                System.out.println("Experiment End with run time:  " + this.experimentRealRunTime); 
        
                // Notify to observers that experiment is done
                setChanged();
                notifyObservers(this.experiment.getExperimentId());

            } else {

                this.experiment.setExperimentRunTime
                    (this.experiment.getExperimentRunTime() - 8);
                // Release equipment back to repository. 
                this.chief.getRepository().releaseEquipment
                    (this.experiment.getRequiredEquipment());

                date = new Date();
                this.experimentRealRunTime -= date.getTime();

                // Sleep for 16 hours ~ 16,000 miliseconds
                try{
                Thread.currentThread().sleep(1600);
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
