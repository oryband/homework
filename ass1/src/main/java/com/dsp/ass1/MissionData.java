package com.dsp.ass1;


public class MissionData  {
    public int remaining;  // Remaining tasks.
    public StringBuilder info;  // PDF data

    public MissionData(int remaining) {
        this.remaining = remaining;
        this.info = new StringBuilder();
    }


    public String getInfo() {
        return info.toString();
    }


    // Add task to result string.
    public void appendTask(String[] task) {
        if (task.length < 3) {
            return;
        } else {
            info.append("\n" + task[1] + ": " + task[2] + "  " + task[3]);
        }
    }
}
