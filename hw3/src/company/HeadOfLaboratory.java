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
        this.executor = Executors.newFixedThreadPool(numOfScientists);
        this.experiments = null;
    }




}
