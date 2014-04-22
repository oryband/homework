package com.dsp.ass1;

public class MissionData  {
        private int number;
        private StringBuilder info;


        public MissionData(int number) {
            this.number = number;
            this.info = new StringBuilder();
        }


        public void setNumber(int number) {
            this.number = number;
        }


        public int getNumber() {
            return number;
        }


        public void decrease() {
            this.number--;
        }


        public String getInfo() {
            return info.toString();
        }


        // create the line for each pdf in the success results file.
        public void stringAppendSucc(String[] split) {
            info.append("\n<" + split[1] + "> :  " + split[2] + "  " + split[3]);
        }


        // create the line for each pdf in the success results file.
        public void stringAppendFailed(String[] split) {
            info.append("\n<" + split[1] + "> :  " + split[2] + "  < " + split[3] + " >");
        }
}
