package utilities;

import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.Scanner;
import java.util.ArrayList;

public class util{

    // Get lines from files.
    public ArrayList<String> getLines(String filePath) throws IOException {

        String thisLine; // TODO all  need to be private - geting error.
        BufferedReader br = null;
        ArrayList<String> lines = new ArrayList<String>();
        try {
            br  = new BufferedReader(new FileReader("EquipmentForSale.txt"));
            while ((thisLine = br.readLine()) != null ) {
                lines.add(thisLine);
                System.out.println(lines.get( lines.size()-1));
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
    }

    // Croping data from lines and importing to data structures.
    public void saveLines(ArrayList<String> arr, int flag) {

        switch(flag) {
            // InitialData.txt
            case 1: 
                break;
                //EquipmentForSale.txt
            case 2: 
                break;
                //ExperimentsList.txt
            case 3: 
                break;
                //LaboratoriesForSale.txt
            case 4: 
                break;
                //ScientistsForPurchase.txt
            case 5: 
                break;

            default: 
                System.out.println("Invaild input referance to file");
                break;
        }



    }
}


