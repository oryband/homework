package company;

import utilities.*;
import java.io.*;
import java.util.*;


public class Driver{


public static void main(String args[]){

        Util u = new Util();  

        // 1 error - need to chanfe arguments send to getDataFromInizialData
        //  u.getDataFromInizialData("InitialData.txt");


        ArrayList<Experiment> experiments = new ArrayList<Experiment>();
        experiments = u.getDataFromExperimentsList("ExperimentsList.txt");

        System.out.println("all good , need to print" ); 

        Iterator<Experiment> it = experiments.iterator();

        while(it.hasNext()) {
       //print to string 
        System.out.println(it.next().toString());
        }
    }

}
