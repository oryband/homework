//package company;

import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.HashMap; 


public class Util{

    // Get lines from files.
    public ArrayList<String> getLines(String filePath) throws IOException {

        String thisLine; // TODO all  need to be private - geting error.
        BufferedReader br = null;
        ArrayList<String> lines = new ArrayList<String>();
        try {
            br  = new BufferedReader(new FileReader(filePath));
            while ((thisLine = br.readLine()) != null ) {
                lines.add(thisLine);
            }
        } catch (IOException e){
            System.err.println("Error: " + e);
            e.printStackTrace();

        } finally {
            try{ 
                br.close();
            }
            catch (IOException ex){
                System.err.println("Error: closing file " + ex);
                ex.printStackTrace();
            }
        }

        return lines;
    }

    // need to return ArrayList
    public ArrayList<String> divideLinesByDelimiter(ArrayList<String> arr,
            String delimeter){

        ArrayList<String> lines = new ArrayList<String>();
        Iterator<String> itr = arr.iterator();

        // Iterate all lines
        while (itr.hasNext()) {
            Scanner s = new Scanner(itr.next());
            if (delimeter.equals("TAB") == true) {
                s.useDelimiter("\t");   // Use delimeter Tab.
            }
            else if (delimeter.equals("COMMA") == true) {
                s.useDelimiter("[, ]");    // User delimeter comma.
            }
            else if (delimeter.equals("SPACE") == true) {
                s.useDelimiter("\\s");    // User delimeter comma.
            }

            while (s.hasNext()) {  // Divide each line by tab and add to arr.
                lines.add(s.next());
            }
            s.close();
        }
        return lines;
    }

    public void getDataFromInizialData(String filePath,
            Statistics stat,
            Repository repo,
            ArrayList<HeadOfLaboratory> headsOfLaboratory){

        ArrayList<String> words = new ArrayList<String>();
        try{
            words = divideLinesByDelimiter(getLines(filePath),"TAB");
        } catch (IOException e) {
            System.err.println("Error: Can't get data from file: "+ e);
            e.printStackTrace();
        }

        for (int i=0; i < words.size() ; i++) {
            // Assign Budget
            if (words.get(i).equals("Budget") == true) {
                stat.setBudget(Integer.parseInt(words.get(i+1))); // Sets budget.
                //TODO TEST!
                //System.out.println(words.get(i+1));
            }
            // Assign equipment to repository 
            if (words.get(i).equals("Repository") == true) {
                while (words.get(i+1).equals("Laboratories") == false) {
                    repo.getRepository().put(new String(words.get(i+1)),
                            new Integer(Integer.parseInt(words.get(i+2))));
                    //TODO TEST!
                   // System.out.println(words.get(i+1));
                   // System.out.println(words.get(i+2));
                    i += 2;
                }
            }
            if (words.get(i).equals("Laboratories") == true) {
                while (i != words.size()-1){

                    headsOfLaboratory.add(new HeadOfLaboratory(
                      words.get(i+1),
                      words.get(i+2),
                      Integer.parseInt(words.get(i+3))));
                    //TODO TEST!
                    //System.out.println(words.get(i+1));
                    //System.out.println(words.get(i+2));
                    //System.out.println(words.get(i+3));
                    i += 3;
                }
            }
        }
    }

    public ArrayList<Experiment> getDataFromExperimentsList(String filePath){

        ArrayList<String> lines = new ArrayList<String>();
        ArrayList<Experiment> experiments = new ArrayList<Experiment>();
        Iterator<String> it = null; 
        try{
            lines = getLines(filePath);
        } catch (IOException e) {
            System.err.println("Error: Can't get data from file: "
                    + filePath + e);
            e.printStackTrace();
        }
        //TODO TESTT
        it = lines.iterator();
        while(it.hasNext()) {

            ArrayList<String> words = new ArrayList<String>();
            ArrayList<String> analyzeWords0 = new ArrayList<String>();
            analyzeWords0.add(it.next());
            
            words = divideLinesByDelimiter(analyzeWords0,"TAB");

            String id = new String(words.get(0));

            ArrayList<Integer> preExperiments = new ArrayList<Integer>();
            ArrayList<String> dividedWords = new ArrayList<String>();
            ArrayList<String> analyzeWords1 = new ArrayList<String>();
            analyzeWords1.add(words.get(1));
            dividedWords = divideLinesByDelimiter(analyzeWords1,
                    "SPACE");

            // Convert String array to int array
            for (int k=0 ; k < dividedWords.size() ; k++) {
                // if there is no preExperiments required the ArrayList will be empty
                if (dividedWords.get(k).equals("0") != true ) {
                    preExperiments.add(Integer.parseInt(
                                dividedWords.get(k)));
                }
                //TODO TEST
                //System.out.println("The # is :" + Integer.parseInt(dividedWords.get(k)));
            }

            String specialization = new String(words.get(2));

            ArrayList<String> equipments = new ArrayList<String>();
            ArrayList<String> analyzeWords = new ArrayList<String>();
            analyzeWords.add(words.get(3));
                //TODO TEST
                //System.out.println(lines.get(3));

            equipments = divideLinesByDelimiter(analyzeWords,"COMMA");

            HashMap<String,Integer> equipmentsMap = 
                new HashMap<String,Integer>();

            for (int e=0 ; e < equipments.size() ; e += 2) {
                //TODO TEST
                //System.out.println(equipments.size());
                equipmentmap.put(new String(equipments.get(e)),
                        new Integer(Integer.parseInt(equipments.get(e+1))));
            }
           
            int runtime = Integer.parseInt(words.get(4));
            int reward = Integer.parseInt(words.get(5));

            // Creating instance of Experiment and push to array list.
            Experiment experiment = new Experiment(id,
                    preExperiments,
                    specialization,
                    equipmentPackages,
                    runtime,
                    reward,
                    "INCOMPLETE");
            experiments.add(experiment);
        }
        return experiments;
    }

    public HashMap<String, ArrayList<EquipmentPackage>>
        getDataFromEquipmentForSale(String equipment) {
    
        ArrayList<String> lines = new ArrayList<String>();
        HashMap<String, ArrayList<EquipmentPackage>> equipmentPackages =
            new HashMap<String, ArrayList<EquipmentPackage>>(); 

        Iterator<String> it = null; 
        try{
            lines = getLines(equipment);
        } catch (IOException e) {
            System.err.println("Error: Can't get data from file: "
                    + equipment + e);
            e.printStackTrace();
        }
        
        it = lines.iterator();
        while (it.hasNext()) {

            // Get words from line
            ArrayList<String> words = new ArrayList<String>();
            ArrayList<String> analyzeWords = new ArrayList<String>();
            analyzeWords.add(it.next());
            words = divideLinesByDelimiter(analyzeWords,"TAB");
            
            // Check for double entries.
            if (equipmentPackages.containsKey(words.get(0))) {

                equipmentPackages.get(words.get(0)).add(
                        new EquipmentPackage(words.get(0),
                            Integer.parseInt(words.get(1)),
                            Integer.parseInt(words.get(2))));
            } else {

                equipmentPackages.put(new String(words.get(0)),
                        new ArrayList<EquipmentPackage>());
                if (equipmentPackages.containsKey(words.get(0))) {

                    equipmentPackages.get(words.get(0)).add(
                                new EquipmentPackage(words.get(0),
                                    Integer.parseInt(words.get(1)),
                                    Integer.parseInt(words.get(2))));
                            } else {
                                System.out.println("Error: Can't find specific etry");
                            }
            }
        }
        //TODO TEST
       //System.out.println(equipmentPackages.values());
        return equipmentPackages;
    }
    
    public HashMap<String, ArrayList<Laboratory>>
        getDataFromLaboratoriesForSale(String laboratories) {
    
        ArrayList<String> lines = new ArrayList<String>();
        HashMap<String, ArrayList<Laboratory>> labs =
            new HashMap<String, ArrayList<Laboratory>>(); 

        Iterator<String> it = null; 
        try{
            lines = getLines(laboratories);
        } catch (IOException e) {
            System.err.println("Error: Can't get data from file: "
                    + laboratories + e);
            e.printStackTrace();
        }
        
        it = lines.iterator();
        while (it.hasNext()) {

            // Get words from line
            ArrayList<String> words = new ArrayList<String>();
            ArrayList<String> analyzeWords = new ArrayList<String>();
            analyzeWords.add(it.next());
            words = divideLinesByDelimiter(analyzeWords,"TAB");
            
            // Check for double entries.
            if (labs.containsKey(words.get(1))) {

                labs.get(words.get(1)).add(
                        new Laboratory(words.get(0),
                            words.get(1),
                            Integer.parseInt(words.get(2)),
                            Integer.parseInt(words.get(3))));
            } else {

                labs.put(new String(words.get(1)),
                        new ArrayList<Laboratory>());
                if (labs.containsKey(words.get(1))) {

                    labs.get(words.get(1)).add(
                                new Laboratory(words.get(0),
                                    words.get(1),
                                    Integer.parseInt(words.get(2)),
                                    Integer.parseInt(words.get(3))));
                            } else {
                                System.out.println("Error: Can't find specific etry");
                            }
            }
        }
        //TODO TEST
       //System.out.println(equipmentPackages.values());
        return labs;
    }
    
    public HashMap<String, ArrayList<Scientist>>
        getDataFromScientistsForSale(String scientistsFilePath) {
    
        ArrayList<String> lines = new ArrayList<String>();
        HashMap<String, ArrayList<Scientist>> scientists =
            new HashMap<String, ArrayList<Scientist>>(); 

        Iterator<String> it = null; 
        try{
            lines = getLines(scientistsFilePath);
        } catch (IOException e) {
            System.err.println("Error: Can't get data from file: "
                    + scientists + e);
            e.printStackTrace();
        }
        
        it = lines.iterator();
        while (it.hasNext()) {

            // Get words from line
            ArrayList<String> words = new ArrayList<String>();
            ArrayList<String> analyzeWords = new ArrayList<String>();
            analyzeWords.add(it.next());
            words = divideLinesByDelimiter(analyzeWords,"TAB");

            // Check for double entries.
            if (scientists.containsKey(words.get(1))) {

                scientists.get(words.get(1)).add(
                        new Scientist(words.get(0),
                            words.get(1),
                            Integer.parseInt(words.get(2))));
            } else {

                scientists.put(new String(words.get(1)),
                        new ArrayList<Scientist>());
                if (scientists.containsKey(words.get(1))) {

                    scientists.get(words.get(1)).add(
                            new Scientist(words.get(0),
                                words.get(1),
                                Integer.parseInt(words.get(2))));
                } else {
                    System.out.println("Error: Can't find specific etry");
                            }
            }
        }
        //TODO TEST
       //System.out.println(equipmentPackages.values());
        return scientists;
    }


}

