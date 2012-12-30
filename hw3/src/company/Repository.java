package company;

import java.io.*;
import java.util.HashMap; 

public class Repository{


    private HashMap<String,Integer> equipment;


    // Constructor
    public Repository(){
        this.equipment = new HashMap<String,Integer>();
    }

    // Getters
    public HashMap<String,Integer> getRepository(){
        return equipment;
    }

    // TODO 
    public aquireEquipment(HashMap<String,Integer> equipments){

    }

    // TODO 
    public releaseEquipment(HashMap<String,Integer> equipments){}



    public String toString(){

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");
        
        result.append("______________________________________" + NEW_LINE);
        result.append("           ---Repository---: " + NEW_LINE);
        result.append("EquipmentPackage data: " + NEW_LINE);
        result.append(this.equipment.toString() + NEW_LINE);
        return result.toString();
    }
}
