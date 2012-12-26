package company;

import java.io.*;
import java.util.*;

public class ChiefScientist{

    private ArrayList<HeadOfLaboratory> laboratories;
    private ArrayList<Experiment> experiments;
    private Statistics statistics;
    private ScienceStore store;
    private Repository repository;
    private ChiefScientistAssistant chiefAssistant;


    public ChiefScientist(ArrayList<HeadOfLaboratory> laboratories,
                          ArrayList<Experiment> experiments,
                          Statistics statistics,
                          ScienceStore store,
                          Repository repository,
                          ChiefScientistAssistant chiefAssistant) {

        this.laboratories = laboratories;
        this.experiments = experiments;
        this.statistics = statistics;
        this.store = store;
        this.repository = repository;
        this.chiefAssistant = chiefAssistant;
            }


}
