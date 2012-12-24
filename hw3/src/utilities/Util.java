package utilities;

import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.Iterator;


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

        return lines;
    }

    // need to return ArrayList
    public ArrayList<String> divideLinesByTab(ArrayList<String> arr){

        System.out.println("in Divide");
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
        return arr;
    }
}
