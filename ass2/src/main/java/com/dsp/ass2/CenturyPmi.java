package com.dsp.ass2;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

import org.apache.hadoop.io.WritableComparable;


public class CenturyPmi implements WritableComparable<CenturyPmi> {
    public String century = "",
           PMI = "";


    public void set(String century, String PMI) {
        this.century = century;
        this.PMI = PMI;
    }


    // Parse century & PMI from stream.
    public void readFields(DataInput in) throws IOException {
        String[] split = in.readLine().split(Utils.delim);
        this.century = split[0];
        this.PMI = split[1];
    }


    public void write(DataOutput out) throws IOException {
        out.writeBytes(century + Utils.delim + PMI);

    }

    // TODO check if it works!
    // Pairs with larger PMI are placed before smaller PMj.
    public int compareTo(CenturyPmi other) {
        double myPMI = Double.parseDouble(this.PMI),
               otherPMI = Double.parseDouble(other.PMI);

        return (myPMI > otherPMI ? -1 : (myPMI == otherPMI ? 0 : 1));
    }
}
