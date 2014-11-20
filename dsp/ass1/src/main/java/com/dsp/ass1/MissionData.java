package com.dsp.ass1;


public class MissionData  {
    public int remaining;  // Remaining tasks.
    public StringBuilder data;  // PDF data

    public MissionData(int remaining) {
        this.remaining = remaining;
        this.data = new StringBuilder();
    }


    public String getData() {
        return data.toString();
    }


    // Add task to result string.
    public void appendTask(String[] task) {
        if (task.length < 3) {
            return;
        } else {
            data.append("\n" + task[1] + ": " + task[2] + "  " + task[3]);
        }
    }
}
