package com.dsp.ass2;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

import org.apache.hadoop.io.WritableComparable;


public class DecadePmi implements WritableComparable<DecadePmi> {
    public String decade = "",
           PMI = "";


    public void set(String decade, String PMI) {
        this.decade = decade;
        this.PMI = PMI;
    }


    // Parse decade & PMI from stream.
    public void readFields(DataInput in) throws IOException {
        String[] split = in.readLine().split(Utils.delim);
        this.decade = split[0];
        this.PMI = split[1];
    }


    public void write(DataOutput out) throws IOException {
        out.writeBytes(decade + Utils.delim + PMI);
    }


    // Pairs with larger PMI are placed before smaller PMI.
    public int compareTo(DecadePmi other) {
        double myPMI = Double.parseDouble(this.PMI),
               otherPMI = Double.parseDouble(other.PMI);

        return (myPMI > otherPMI ? -1 : (myPMI == otherPMI ? 0 : 1));
    }
}
