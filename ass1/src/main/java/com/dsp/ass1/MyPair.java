package com.dsp.ass1;

public class MyPair  {
        private int first; //first member of pair
        private String second; //second member of pair

        public MyPair(int first, String second) {
            this.first = first;
            this.second = second;
        }

        public void setFirst(int first) {
            this.first = first;
        }

        public void setSecond(String second) {
            this.second = second;
        }

        public int getFirst() {
            return first;
        }

        public void decrease() {
            this.first--;
        }

        public String getSecond() {
            return second;
        }
}
