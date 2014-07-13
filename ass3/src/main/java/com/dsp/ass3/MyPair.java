package com.dsp.ass3;

public class MyPair implements Comparable<MyPair> {
    public int index;
    public String occ;


    public MyPair(int otherIndex, String otherOcc) {
        this.index = otherIndex;
        this.occ = new String(otherOcc);
    }

    @Override
    public int compareTo(MyPair other) {
        if (index < other.index) {
            return -1;
        }
        if (index > other.index) {
            return 1;
        }
        return 0;
    }

    @Override
    public String toString() {
        return index + " " + occ;
    }
}
