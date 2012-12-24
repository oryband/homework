package company;

import utilities.Util;
import java.io.*;


public class Driver{


public static void main(String args[]){

        Util u = null;// new Util();  

        try{
            u = new Util();
            u.divideLinesByTab(u.getLines("InitialData.txt"));
        } catch (IOException e){
          System.err.println("error: " + e);
        }
}

}
