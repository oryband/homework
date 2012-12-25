package company;

import java.io.*;
import java.util.concurrent.*;
import java.util.ArrayList;


public class HeadOfLaboratory{

    private String name;
    private String specialization;
    private int numberOfScientists;
    private ArrayList<RunnableExperiment> experiments;
    private ExecutorService executor;


    public  HeadOfLaboratory(String headLabName,
                             String special,
                             int numOfScientists) {
        this.name = new String(headLabName);
        this.specialization = new String(special);
        this.numberOfScientists = numOfScientists;
        this.executor = Executors.newFixedThreadPool(numOfScientists);
        this.experiments = null;
    }
    
    public String toString(){

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");
        
        result.append("______________________________________" + NEW_LINE);
        result.append("           ---Head Of Laboratory---: " + NEW_LINE);
        result.append("Name: " + this.name + NEW_LINE);
        result.append("Specialization: " + this.specialization + NEW_LINE);
        result.append("Number Of Scientists: " + this.numberOfScientists + NEW_LINE);
        return result.toString();
    }




}
