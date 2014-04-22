package com.dsp.ass1;

public class MissionData  {
        private int number;
        private String info;

        public MissionData(int number) {
            this.number = number;
            this.info = "";
        }

        public void setNumber(int number) {
            this.number = number;
        }

        public void setInfo(String info) {
            this.info = new String (info);
        }

        public int getNumber() {
            return number;
        }

        public void decrease() {
            this.number--;
        }

        public String getInfo() {
            return new String (info);
        }
}
