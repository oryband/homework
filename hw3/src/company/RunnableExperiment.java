/** @author Eldar Damari, Ory Band */

package company;

import java.util.Observable;
import java.util.Date;
import java.lang.Runnable;


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

        System.out.println(
                "#" + this.experiment.getId() + " started.");

        // Experiment still in progress
        while (experiment.getCurrentRunTime() > 0) {

            this.experimentRealRunTime += new Date().getTime();

            Repository repo = this.chief.getRepository();

            repo.acquireEquipment(this.experiment.getRequiredEquipment()); 

            // Sleep 8 hours (1 hour = 100 ms.)
            try {
                Thread.sleep(800);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            // Return equipment taken for the day.
            this.chief.getRepository().releaseEquipment(
                    this.experiment.getRequiredEquipment());

            if (this.experiment.getCurrentRunTime() <= 8) {

                this.experiment.setRealRunTime(0);

                this.experimentRealRunTime = 
                    new Date().getTime() - this.experimentRealRunTime;

                // Reward calculation.
                if (this.experimentRealRunTime <= 
                        this.experiment.getRequiredRunTime() * 115) {

                    // 100% of reward gained.
                    this.chief.getStatistics().addReward(
                            this.experiment.getReward());

                } else {  // 10% of reward gained.
                    this.chief.getStatistics().addReward(
                            this.experiment.getReward() / 10);
                }

                System.out.println(
                        "#" + this.experiment.getId() + " ended: "
                        + this.experimentRealRunTime + " hours.");
        
                // Notify observers (ChiefScientist) that experiment is complete.
                setChanged();
                notifyObservers(this.experiment.getId());
            } else {
                this.experiment.setRealRunTime(
                        this.experiment.getCurrentRunTime() - 8);

                this.experimentRealRunTime -= new Date().getTime();

                // Sleep for 16 hours = 1600 ms.
                try {
                    Thread.sleep(1600);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }


    public Experiment getExperiment() {
        return this.experiment;
    }


    public String toString() {

        StringBuilder result = new StringBuilder();

        String N = System.getProperty("line.separator");

        result.append(N);
        result.append("Runnable Experiment:" + N);
        result.append("Real Run Time: " + this.experimentRealRunTime + N);
        result.append(this.experiment.toString() + N);

        return result.toString();
    }
}
