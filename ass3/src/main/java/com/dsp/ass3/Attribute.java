package com.dsp.ass3;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;

public class Attribute {

    public static void main(String[] args){
        System.out.println("Starting...");
        try {
            BufferedReader br = new BufferedReader(new FileReader(args[0]));
            PrintWriter writer = new PrintWriter("attributes", "UTF-8");
            String line = br.readLine();
            while (line != null) {
                writer.println("@ATTRIBUTE " + weka.core.Utils.quote(line) + " INTEGER");
                line = br.readLine();
            }
            br.close();
            writer.close();
        }
        catch(FileNotFoundException e) {
            System.out.println(e.getMessage());
            return;
        } catch (IOException e) {
            e.getMessage();
            return;
        }
        System.out.println("Done...");
    }
}
