package company;

import java.io.*;
import java.util.*;


public class ChiefScientistAssistant implements Runnable{

    private ArrayList<RunnableExperiment> experiments;
    private int completeExperiments;
    private ChiefScientist chief;

    // Constructor
    public ChiefScientistAssistant(ArrayList<Experiment> experimentsToRun,
                                   ChiefScientist chief) {
        
        Iterator<Experiment> it = experimentsToRun.iterator();

        while (it.hasNext()) {
            experiments.add(new RunnableExperiment(it.next(), chief));
        }
        // will be indicator for finishing scaning experiments.
        this.completeExperiments = 0;
        this.chief = chief;
    }

    public void run() {

        // Program Cycle
        while (this.completeExperiments != this.experiments.size())
            // Scaning experiments and find experiments with 
            // no preExperiments required..
            Iterator<RunnableExperiment> it = this.experiments.iterator();

            while(it.hasNext()) {
               
                // Copy of experiment to work with - can't work with Iterator
                Experiment experimentItr = it.next();
                
                if (experimentItr.getExperiment()
                        .getExperimentPreRequirementsExperiments().size() == 0) {
                
                    // check status
                    if (experimentItr.getExperiment()
                            .getExperimentStatus()
                                .equals("INCOMPLETE") == true) {
                        // search laboratory and add Experiment
                        // later: claculate effiency and buy new scientists
                        Iterator<HeadOfLaboratory> labIt = 
                            this.chief.getLaboratories().iterator();

                        while (labIt.hasNext()) {
                            // Copy of experiment to work with.
                            HeadOfLaboratory laboratoryIt = labIt.next();

                            // Find laboratory with same specialization.
                            if (laboratoryIt.getSpecialization()
                                    .equals(experimentItr
                                        .getExperimentSpecialization()) == true) {

                                // change status of experiment to InProgress.
                                experimentItr.setExperimentStatus("INPROGRESS");
                                // TODO should it be send with new??
                                laboratoryIt.addExperimentToExecute(experimentItr);
                            } else { // TODO HERE - what to do if didn't find Laboratory.


                            }



                        }


                    } else { // Experiment is Complete or InProgress


                    }



                } else {  // pre experiments required


                }




            }




    }

    public String toString(){

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");
        Iterator<RunnableExperiment> it = experiments.iterator();

        result.append("______________________________________" + NEW_LINE);
        result.append("           ---Chief Scientist Assistant---: " + NEW_LINE);
        while (it.hasNext()) {
        result.append(it.next().toString() + NEW_LINE);
        }

        return result.toString();
    }

}
