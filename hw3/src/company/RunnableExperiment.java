//package company;

import java.io.*;
import java.util.*;


public class RunnableExperiment extends Observable implements Runnable {

    private Experiment experiment;
    private long experimentRealRunTime;
    private Date date;
    private ChiefScientist chief;
    
    // Constructor
    public RunnableExperiment(Experiment exp, ChiefScientist chief) {
        this.experiment = exp;
        addObserver(chief); 
        this.experimentRealRunTime= 0;
        this.chief = chief;
    }

    public void run(){

        // Experiment still in progress
        while (experiment.getExperimentRunTime() != 0) {

            // get starting time of experiment
            this.date = new Date();
            this.experimentRealRunTime += date.getTime();

            // Acuire equipment from repository
            this.chief.getRepository().aquireEquipment(this.experiment.getExperimentRequiredEquipments()); 

            // Sleep 8 hours
            try{
            Thread.currentThread().sleep(800);  // Sleep 8 hours ( 1 hour = 100 miliseconds ).
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            if (this.experiment.getExperimentRunTime() <= 8) {

                this.experiment.setExperimentRunTime(0);

                //Release equipment back to repo - if someone is buying right  now?
                //problem with returning & purchasing? - check sync carefully in repo!!

                this.chief.getRepository().releaseEquipment
                    (this.experiment.getExperimentRequiredEquipments());

                date = new Date();
                this.experimentRealRunTime = 
                    date.getTime() - this.experimentRealRunTime;

                // reward claculation and updating statistics
                if (this.experimentRealRunTime <= 
                        ((this.experiment.getExperimentRunTime() / 100) * 115)){
                    // reward gained.
                    this.chief.getStatistics().addReward(this.experiment.getExperimentReward());

                } else {
                    // 10% of reward gained.
                    this.chief.getStatistics().addReward((this.experiment.getExperimentReward() / 100) * 10);

                }

                // Notify to observers that experiment is done
                notifyObservers(this.experiment.getExperimentId());

            } else {

                this.experiment.setExperimentRunTime
                    (this.experiment.getExperimentRunTime() - 8);
                // Release equipment back to repository. 
                this.chief.getRepository().releaseEquipment
                    (this.experiment.getExperimentRequiredEquipments());

                date = new Date();
                this.experimentRealRunTime -= date.getTime();

                // Sleep for 16 hours ~ 16,000 miliseconds
                try{
                Thread.currentThread().sleep(16000);
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
}
