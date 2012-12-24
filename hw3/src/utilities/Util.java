package utilities;

import stat.*;
import company.*;

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
    public ArrayList<String> divideLinesByTab(ArrayList<String> arr){

        String word = new String();
        ArrayList<String> lines = new ArrayList<String>();
        Iterator<String> itr = arr.iterator();

        // Iterate all lines
        while (itr.hasNext()) {
            Scanner s = new Scanner(itr.next());
            s.useDelimiter("\\s*\t*\\s");

            while (s.hasNext()) {  // Divide each line by tab and add to arr.
                lines.add(s.next());
            }
            s.close();
        }
        return lines;
    }

public void getDataFromInizialData(String filePath,
                                   Satistics stat,
                                   Repository repo,
                                   ArrayList<HeadOfLaboratory> headsOfLaboratory){

        ArrayList<String> words = new ArrayList<String>();
        try{
        words = divideLinesByTab(getLines(filePath));
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
                    repo.getEquipment().put(new String(words.get(i+1)),
                                            new Integer(Integer.parseInt(words.get(i+2))));
                //TODO TEST!
                //System.out.println(words.get(i+1));
                //System.out.println(words.get(i+2));
                    i += 2;
                }
            }
            if (words.get(i).equals("Laboratories") == true) {
                while (i != words.size()-1){
                
                    HeadOfLaboratory head = new HeadOfLaboratory("rr","rr",2);
                    headsOfLaboratory.add(head);
                    /*headsOfLaboratory.add(new HeadOfLaboratory(
                                words.get(i+1),
                                words.get(i+2),
                                Integer.parseInt(words.get(i+3))));*/
                //TODO TEST!
                //System.out.println(words.get(i+1));
                //System.out.println(words.get(i+2));
                //System.out.println(words.get(i+3));
                    i += 3;
                }
             }
    }
}

}

