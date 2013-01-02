/** @author Eldar Damari, Ory Band */

import company.*;

import java.util.HashMap;
import java.util.ArrayList;


public class Driver {
    public static void main(String args[]) {

        Util u = new Util();  // File reader.

        // TODO statistics, repos and other objects should be created and handled in chiefScientist.
        Statistics statistics = new Statistics();
        Repository repository = new Repository();
        ArrayList<HeadOfLaboratory> laboratories = new ArrayList<HeadOfLaboratory>();
        ArrayList<Experiment> experiments;
        HashMap<String, ArrayList<EquipmentPackage>> equipmentForSale;
        HashMap<String, ArrayList<Laboratory>> laboratoryForSale;
        HashMap<String, ArrayList<Scientist>> scientistsForSale;

        u.getDataFromInizialData("InitialData.txt", statistics, repository, laboratories);

        experiments = u.getDataFromExperimentsList("ExperimentsList.txt");
        equipmentForSale = u.getDataFromEquipmentForSale("EquipmentForSale.txt");
        laboratoryForSale = u.getDataFromLaboratoriesForSale("LaboratoriesForSale.txt");
        scientistsForSale = u.getDataFromScientistsForSale("ScientistsForPurchase.txt");

        ScienceStore store = new ScienceStore(
                equipmentForSale, scientistsForSale, laboratoryForSale);
 

        ChiefScientist chief = new ChiefScientist(
                laboratories, experiments, statistics, store, repository);

        chief.simulateCompany();  // Run the show.
    }
}
