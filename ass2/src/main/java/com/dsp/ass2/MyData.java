package com.dsp.ass2;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;


import org.apache.hadoop.io.WritableComparable;


public class MyData implements WritableComparable<MyData> {
    public String century;
    public String PMI;

    public MyData() {
        this.century = "";
        this.PMI = "";
    }

    public MyData(String century, String PMI) {
        this.century = century;
        this.PMI = PMI;
    }

    public void set(String century, String PMI) {
        this.century = century;
        this.PMI = PMI;
    }

    public void readFields(DataInput in) throws IOException {
        String[] split = in.readLine().split(Utils.delim);
        this.century = split[0];
        this.PMI = split[1];

    }

    public void write(DataOutput out) throws IOException {
        out.writeBytes(century + Utils.delim + PMI);

    }

    // TODO check if it works!
    public int compareTo(MyData other) {
            double myPMI = Double.parseDouble(this.PMI);
            double otherPMI = Double.parseDouble(other.PMI);
            return (myPMI > otherPMI ? -1 : (myPMI == otherPMI ? 0 : 1));
    }



}
