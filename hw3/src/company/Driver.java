//package company;

import java.io.*;
import java.util.*;


public class Driver {
    public static void main(String args[]) {

        // utilities instance - reads files.
        Util u = new Util();  

        // TODO statistics, repos and other objects should be created and handled in chiefScientist.
        Statistics statistics = new Statistics();
        Repository repository = new Repository();
        ArrayList<HeadOfLaboratory> headsOfLaboratory = 
            new ArrayList<HeadOfLaboratory>();

        // for ExperimentsList
        ArrayList<Experiment> experiments = new ArrayList<Experiment>();

        // for ScienceStore
        HashMap<String, ArrayList<EquipmentPackage>> equipmentForSale = 
            new HashMap<String, ArrayList<EquipmentPackage>>();
        HashMap<String, ArrayList<Laboratory>> laboratoryForSale= 
            new HashMap<String, ArrayList<Laboratory>>();
        HashMap<String, ArrayList<Scientist>> scientistsForSale= 
            new HashMap<String, ArrayList<Scientist>>();


        // Read data from: InitialData.txt
        u.getDataFromInizialData(
                "InitialData.txt",
                statistics,
                repository,
                headsOfLaboratory);


        // TODO delete us!
        // Print Tests
        /*System.out.println("Printing InitialData : " ); 

          System.out.println(statistics.toString());
          System.out.println(repository.toString());

          Iterator<HeadOfLaboratory> itr = headsOfLaboratory.iterator();

          while(itr.hasNext()) {
        //print to string 
        System.out.println(itr.next().toString());
        }*/


        experiments = u.getDataFromExperimentsList("ExperimentsList.txt");


        // TODO delete us
        //Print Tests
        /*System.out.println("all good , need to print" ); 
          Iterator<Experiment> it = experiments.iterator();
          while(it.hasNext()) {
        //print to string 
        System.out.println(it.next().toString());
        }*/


        equipmentForSale = u.getDataFromEquipmentForSale("EquipmentForSale.txt");

        // TODO delete us
        // Print Tests
        //System.out.println(equipmentForSale.values());


        laboratoryForSale = u.getDataFromLaboratoriesForSale("LaboratoriesForSale.txt");

        // TODO delete us
        // Print Tests
        //System.out.println(laboratoryForSale.values());


        scientistsForSale = u.getDataFromScientistsForSale("ScientistsForPurchase.txt");

        // TODO delete us
        // Print Tests
        //System.out.println(scientistsForSale.values());

        ChiefScientist chief = new ChiefScientist(
                headsOfLaboratory,
                experiments, 
                statistics,
                new ScienceStore(
                    equipmentForSale, scientistsForSale, laboratoryForSale),
                repository);

        // TODO delete us
        // Print Tets
        //System.out.println(chief.toString());

        chief.simulate();
    }
}
