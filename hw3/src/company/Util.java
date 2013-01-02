/**
 * Responsible for extracting data from .txt files.
 *
 * @author Eldar Damari, Ory Band.
 */

package company;

import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;

import java.util.Scanner;

import java.util.ArrayList;
import java.util.HashMap; 


// Singleton.
public enum Util {

    INSTANCE;  // Singleton pattern.


    /**
     * Reads lines from files.
     *
     * @param filePath file to read from.
     *
     * @return lines from file.
     */
    public ArrayList<String> getLines(String filePath) throws IOException {

        String thisLine;
        BufferedReader br = null;
        ArrayList<String> lines = new ArrayList<String>();

        try {
            br = new BufferedReader(new FileReader(filePath));

            while ((thisLine = br.readLine()) != null) {
                lines.add(thisLine);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try { 
                br.close();
            }
            catch (IOException e) {
                e.printStackTrace();
            }
        }

        return lines;
    }


    /**
     * Divides lines by a delimeter given as argument.
     *
     * @param lines lines to divide.
     * @param delimeter delimeter to split by.
     */
    public ArrayList<String> split(
            ArrayList<String> inputLines, String delimeter) {

        ArrayList<String> outputLines = new ArrayList<String>();

        for (String line : inputLines) {
            Scanner s = new Scanner(line);

            if (delimeter.equals("TAB")) {
                s.useDelimiter("\t");
            } else if (delimeter.equals("COMMA")) {
                s.useDelimiter("[, ]");
            } else if (delimeter.equals("SPACE")) {
                s.useDelimiter("\\s");
            }

            while (s.hasNext()) {
                outputLines.add(s.next());
            }

            s.close();
        }

        return outputLines;
    }


    /**
     * Reads data from initaldata.txt file.
     *
     * @param filePath .txt file path.
     * @param statistics Statistics object.
     * @param repository Repository object.
     * @param laboratories list of [HeadOfLaboratory].
     */
    public void readInitialData(
            String filePath,
            Statistics statistics,
            Repository repository,
            ArrayList<HeadOfLaboratory> laboratories) {

        ArrayList<String> words = new ArrayList<String>();

        try {
            words = split(getLines(filePath), "TAB");
        } catch (IOException e) {
            e.printStackTrace();
        }

        for (int i=0; i < words.size(); i++) {

            if (words.get(i).equals("Budget")) {
                statistics.setBudget(
                        Integer.parseInt(words.get(i+1)));
            }

            // Reads equipment.
            if (words.get(i).equals("Repository")) {
                while ( ! words.get(i+1).equals("Laboratories") ) {

                    repository.getRepository().put(
                            new String(words.get(i+1)),
                            new Integer(Integer.parseInt(words.get(i+2))));

                    i += 2;
                }
            }

            // Reads labs.
            if (words.get(i).equals("Laboratories")) {
                while (i != words.size() -1) {

                    laboratories.add(
                            new HeadOfLaboratory(
                                words.get(i+1),
                                words.get(i+2),
                                Integer.parseInt(words.get(i+3))));

                    i += 3;
                }
            }
        }
    }


    /**
     * Reads data from experimentlist.txt
     *
     * @param filePath file path.
     *
     * @return experiments read from file.
     */
    public ArrayList<Experiment> readExperimentsList(String filePath){
        ArrayList<String> lines = new ArrayList<String>();
        ArrayList<Experiment> experiments = new ArrayList<Experiment>();

        try {
            lines = getLines(filePath);
        } catch (IOException e) {
            e.printStackTrace();
        }

        for (String line : lines) {

            ArrayList<String> words = new ArrayList<String>();

            ArrayList<String> analyzeWords0 = new ArrayList<String>();
            analyzeWords0.add(line);
            
            words = split(analyzeWords0,"TAB");

            String id = new String(words.get(0));

            ArrayList<Integer> preExperiments = new ArrayList<Integer>();

            ArrayList<String> splitWords    = new ArrayList<String>(),
                              analyzeWords1 = new ArrayList<String>();

            analyzeWords1.add(words.get(1));

            splitWords = split(analyzeWords1, "SPACE");

            // Convert String to int.
            for (int k=0 ; k < splitWords.size() ; k++) {
                // if there is no preExperiments required,
                // the list will be empty.
                if ( ! splitWords.get(k).equals("0") ) {
                    preExperiments.add(Integer.parseInt(splitWords.get(k)));
                }
            }

            String specialization = new String(words.get(2));

            ArrayList<String> equipments = new ArrayList<String>();
            ArrayList<String> analyzeWords = new ArrayList<String>();

            analyzeWords.add(words.get(3));

            equipments = split(analyzeWords, "COMMA");

            HashMap<String,Integer> equipmentsMap = 
                new HashMap<String,Integer>();

            for (int e=0 ; e < equipments.size() ; e += 2) {
                equipmentsMap.put(
                        new String(equipments.get(e)),
                        new Integer(Integer.parseInt(equipments.get(e+1))));
            }
           
            int runtime = Integer.parseInt(words.get(4));
            int reward = Integer.parseInt(words.get(5));

            // Create instance and add to list.
            Experiment experiment = new Experiment(
                    id,
                    preExperiments,
                    specialization,
                    equipmentsMap,
                    runtime,
                    reward,
                    "INCOMPLETE");

            experiments.add(experiment);
        }

        return experiments;
    }


    /**
     * Reads equipment for sile file.
     *
     * @param filePath equipment file path.
     *
     * @return equipment hash map { type : [packages] }
     */
    public HashMap<String, ArrayList<EquipmentPackage>>
        readEquipmentForSale(String filePath) {
    
        ArrayList<String> lines = new ArrayList<String>();
        HashMap<String, ArrayList<EquipmentPackage>> equipmentPackages =
            new HashMap<String, ArrayList<EquipmentPackage>>(); 

        try {
            lines = getLines(filePath);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        for (String line : lines) {

            ArrayList<String> words = new ArrayList<String>(),
                              analyzeWords = new ArrayList<String>();

            analyzeWords.add(line);

            words = split(analyzeWords,"TAB");
            
            // Check for double entries.
            if (equipmentPackages.containsKey(words.get(0))) {

                equipmentPackages.get(words.get(0)).add(
                        new EquipmentPackage(words.get(0),
                            Integer.parseInt(words.get(1)),
                            Integer.parseInt(words.get(2))));
            } else {
                equipmentPackages.put(
                        new String(words.get(0)),
                        new ArrayList<EquipmentPackage>());

                if (equipmentPackages.containsKey(words.get(0))) {

                    equipmentPackages.get(words.get(0)).add(
                                new EquipmentPackage(words.get(0),
                                    Integer.parseInt(words.get(1)),
                                    Integer.parseInt(words.get(2))));
                } else {
                    System.out.println(
                            "Util.readEquipmentForSale(): Can't find specific entry");
                }
            }
        }

        return equipmentPackages;
    }

    
    public HashMap<String, ArrayList<Laboratory>>
        readLaboratoriesForSale(String laboratories) {
    
        ArrayList<String> lines = new ArrayList<String>();
        HashMap<String, ArrayList<Laboratory>> labs =
            new HashMap<String, ArrayList<Laboratory>>(); 

        try {
            lines = getLines(laboratories);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        for (String line : lines) {

            ArrayList<String> words = new ArrayList<String>(),
                              analyzeWords = new ArrayList<String>();

            analyzeWords.add(line);

            words = split(analyzeWords,"TAB");
            
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
                    System.out.println(
                            "Util.readLaboratoriesForSale(): Can't find specific entry");
                }
            }
        }

        return labs;
    }
    

    /**
     * Reads scientists for sale file.
     *
     * @param filePath file to read from.
     *
     * @return Scientists hash map { type : [scientists] }
     */
    public HashMap<String, ArrayList<Scientist>>
            readScientistsForSale(String filePath) {

        ArrayList<String> lines = new ArrayList<String>();
        HashMap<String, ArrayList<Scientist>> scientists =
            new HashMap<String, ArrayList<Scientist>>(); 

        try {
            lines = getLines(filePath);
        } catch (IOException e) {
            e.printStackTrace();
        }

        for (String line : lines) {
            ArrayList<String> words = new ArrayList<String>(),
                              analyzeWords = new ArrayList<String>();
            analyzeWords.add(line);
            words = split(analyzeWords,"TAB");

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
                    System.out.println(
                            "Util.readScientistsForSale(): Can't find specific entry");
                }
            }
        }

        return scientists;
    }
}
