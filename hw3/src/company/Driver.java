package company;

import stat.*;
import utilities.*;
import java.io.*;
import java.util.*;


public class Driver{


public static void main(String args[]){

        // Read Data From File: InitialData.txt
        Util u = new Util();  
        Statistics statistics = new Statistics();
        Repository repository = new Repository();
        ArrayList<HeadOfLaboratory> headsOfLaboratory = 
            new ArrayList<HeadOfLaboratory>();

        u.getDataFromInizialData("InitialData.txt",
                                 statistics,
                                 repository,
                                 headsOfLaboratory);

        // Print Tests
        /*System.out.println("Printing InitialData : " ); 

        System.out.println(statistics.toString());
        System.out.println(repository.toString());
        
        Iterator<HeadOfLaboratory> itr = headsOfLaboratory.iterator();

        while(itr.hasNext()) {
       //print to string 
        System.out.println(itr.next().toString());
        }*/


        // Read data from file: ExperimentsList.txt
        ArrayList<Experiment> experiments = new ArrayList<Experiment>();
        experiments = u.getDataFromExperimentsList("ExperimentsList.txt");

        // Print Tests
        /*System.out.println("all good , need to print" ); 
        Iterator<Experiment> it = experiments.iterator();
        while(it.hasNext()) {
       //print to string 
        System.out.println(it.next().toString());
        }*/
    }

}
