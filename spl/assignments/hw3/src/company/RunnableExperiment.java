/** @author Eldar Damari, Ory Band */

package company;

import java.util.Observable;
import java.util.Date;
import java.lang.Runnable;


public class RunnableExperiment extends Observable implements Runnable {

    private Experiment experiment;
    private long runTime;
    private ChiefScientist chief;


    public RunnableExperiment(
            Experiment experiment, ChiefScientist chiefScientist) {

        this.experiment = experiment;
        addObserver(chiefScientist);
        this.runTime = 0;
        this.chief = chiefScientist;
    }


    public void run() {

        System.out.println(
                "#" + this.experiment.getId() + " started.");

        // Experiment still in progress.
        while (experiment.getCurrentRunTime() > 0) {

            this.runTime += new Date().getTime();

            Repository repo = this.chief.getRepository();

            repo.acquireEquipment(this.experiment.getRequiredEquipment()); 

            try {  // Work 8 hours.
                Thread.sleep(800);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            // Return equipment taken for the day.
            this.chief.getRepository().releaseEquipment(
                    this.experiment.getRequiredEquipment());

            if (this.experiment.getCurrentRunTime() > 8) {

                this.experiment.setCurrentRunTime(
                        this.experiment.getCurrentRunTime() - 8);

                this.runTime -= new Date().getTime();

                try {  // Sleep 16 hours.
                    Thread.sleep(1600);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            } else {  // Last hours of experiment.
                this.experiment.setCurrentRunTime(0);

                this.runTime = new Date().getTime() - this.runTime;

                // Reward calculation.
                boolean fullReward;
                if (this.runTime <= this.experiment.getRequiredRunTime() * 115) {

                    // 100% of reward gained.
                    fullReward = true;

                    int reward = this.experiment.getReward();
                    this.chief.getStatistics().addReward(reward);

                } else {
                    // 10% of reward gained.
                    fullReward = false;

                    int reward = this.experiment.getReward();
                    this.chief.getStatistics().addReward(reward / 10);
                }

                System.out.print(
                        "#" + this.experiment.getId() + " ended: "
                        + (this.runTime / 100) + " hours, ");

                if (fullReward) {
                    System.out.println("100% reward gained.");
                } else {
                    System.out.println("10% reward gained.");
                }
        
                // Notify observers (ChiefScientist) that experiment is complete.
                setChanged();
                notifyObservers(this.experiment.getId());
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
        result.append("Run Time: " + this.runTime + N);
        result.append(this.experiment.toString() + N);

        return result.toString();
    }
}
