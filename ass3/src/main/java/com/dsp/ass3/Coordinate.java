package com.dsp.ass3;


public class Coordinate implements Comparable<Coordinate> {
    public long index;
    public String hits;


    public Coordinate(long otherIndex, String otherhits) {
        this.index = otherIndex;
        this.hits = new String(otherhits);
    }


    @Override
    public int compareTo(Coordinate other) {
        if (index < other.index) {
            return -1;
        } else if (index > other.index) {
            return 1;
        } else {
            return 0;
        }
    }

    @Override
    public String toString() {
        return index + Utils.delim + hits;
    }
}
