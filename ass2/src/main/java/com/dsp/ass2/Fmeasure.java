package com.dsp.ass2;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Counter;
import org.apache.hadoop.mapreduce.Counters;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Partitioner;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.Reducer.Context;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import com.dsp.ass2.Count.ReduceClass.N_COUNTER;


public class Fmeasure {

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Fmeasure.class.getName()));

    private static final Text tp = new Text("tp"),
            tn = new Text("tn"),
            fp = new Text("tp"),
            fn = new Text("fn");

    // Write { <w1,w2> : decade, w1, c(w1), c(w1,w2) }
    public static class MapClass extends Mapper<LongWritable, Text, Text, LongWritable> {

        private static final String
        notRelatedLink = "https://s3.amazonaws.com/ory-dsp-ass2/steps/Fmeasure/wordsim-neg.txt",
        RelatedLink = "https://s3.amazonaws.com/ory-dsp-ass2/steps/Fmeasure/wordsim-pos.txt";

        private static double threshold;

        private final static LongWritable one = new LongWritable(1);

        private static Map <String, Boolean> wordsData = new HashMap <String, Boolean>();


        private void initData(Map <String, Boolean> data, Boolean value ,String info) {
            String[] splitInfo = info.split("\n"),
                    splitLine;
            for (int i = 0 ; i < splitInfo.length ; i++) {
                splitLine = splitInfo[i].split("\t");
                if (splitLine.length >= 2) {
                    if (splitLine[0].compareTo(splitLine[1]) < 0) {
                        data.put(splitLine[0] + splitLine[1], value);
                    }
                    else {
                        data.put(splitLine[1] + splitLine[0], value);
                    }
                }
            }
        }

        @Override
        public void setup(Context context) {
            String answer = context.getConfiguration().get("threshold" , null);
            if (answer == null ) {
                logger.severe("Cant find the threshold");
                return;
            }
            threshold = Double.parseDouble(answer);

            initData(wordsData , new Boolean(true), Utils.LinkToString(RelatedLink));
            initData(wordsData, new Boolean(false), Utils.LinkToString(notRelatedLink));
        }


        @Override
        public void map(LongWritable key, Text value, Context context)
                throws IOException, InterruptedException {

            // Fetch words from value.
            String[] words = value.toString().split(Utils.delim);
            String decade = words[0],
                    w1 = words[2],
                    w2 = words[3];

            if (! decade.equals("200")) {
                logger.severe("Unsupported decade: " + decade);
                return;
            }
            // we assume that w1 < w2

            Boolean isInData = wordsData.get(w1 + w2);

            // the pair is not in wordsdata.
            if (isInData == null) {
                return;
            }

            double  PMI = Double.parseDouble(words[1]);

            // the pair is in the related words.
            if (isInData.booleanValue()) {

                if (PMI >= threshold) {
                    context.write(tp, one);
                }
                else {
                    context.write(fn, one);
                }

            }

            else { //  the pair isn't in the related words.
                if (PMI >= threshold) {
                    context.write(fp, one);
                }
                else {
                    context.write(tn, one);
                }
            }
        }
    }


    // Sum every identical count values before sending to reducer.
    public static class CombineClass extends Reducer<Text, LongWritable, Text, LongWritable> {

        @Override
        public void reduce(Text key, Iterable<LongWritable> values, Context context) throws IOException,  InterruptedException {
            long sum = 0;
            for (LongWritable value : values) {
                sum += value.get();
            }
            context.write(key, new LongWritable(sum));
        }
    }

    public static class PartitionerClass extends Partitioner<Text, LongWritable> {
        @Override
        public int getPartition(Text key, LongWritable value, int numPartitions) {
            return (getPartitionNumber(key) % numPartitions);
        }

        private int getPartitionNumber(Text key) {

            if (key.equals(tp)) {
                return 0;
            }

            else if (key.equals(tn)) {
                return 1;
            }

            else if(key.equals(fp)) {
                return 2;
            }

            else {
                return 3;
            }
        }
    }


    public static class ReduceClass extends Reducer<Text, LongWritable, Text, LongWritable> {

        public static enum COUNTER {
            tp, tn, fp , fn
        };

        public void reduce(Text key, Iterable<LongWritable> values, Context context)
                throws IOException, InterruptedException {

            long sum = 0;
            for (LongWritable value : values) {
                sum += value.get();
            }

            context.write(key, new LongWritable(sum));

            updateCounter(key, sum, context);
        }

        private void updateCounter(Text key, long sum, Context context) {

            Counter counter = null;

            if (key.equals(tp)) {
                counter = context.getCounter(COUNTER.tp);
            }

            else if (key.equals(tn)) {
                counter = context.getCounter(COUNTER.tn);
            }

            else if(key.equals(fp)) {
                counter = context.getCounter(COUNTER.fp);
            }

            else if (key.equals(fn)){
                counter = context.getCounter(COUNTER.fn);
            }

            if (counter != null) {
                counter.increment(sum);
            }
        }
    }



public static void main(String[] args) throws Exception {
    Configuration conf = new Configuration();

    conf.set("mapred.reduce.slowstart.completed.maps", "1");

    conf.set("threshold", args[0]);

    Job job = new Job(conf, "Fmeasure");

    job.setJarByClass(Calculate.class);
    job.setMapperClass(MapClass.class);
    job.setPartitionerClass(PartitionerClass.class);
    job.setCombinerClass(CombineClass.class);
    job.setReducerClass(ReduceClass.class);
    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(LongWritable.class);
    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(LongWritable.class);

    boolean result = job.waitForCompletion(true);

    if (result) {

        long fpNum, fnNum, tpNum , tnNum;
        double  recall, precision, f;
        Counters counters = job.getCounters();

        fpNum = counters.findCounter(ReduceClass.COUNTER.fp).getValue();
        fnNum = counters.findCounter(ReduceClass.COUNTER.fn).getValue();
        tpNum = counters.findCounter(ReduceClass.COUNTER.tp).getValue();
        tnNum = counters.findCounter(ReduceClass.COUNTER.tn).getValue();

        precision = tpNum / (tpNum + fpNum);
        recall = tpNum / (tpNum + fnNum);
        f = (2 * precision * recall) / (precision + recall);

        String info = "precision\t" + Double.toString(precision) + "\trecall\t" +
                        Double.toString(recall) + "\tF\t" + Double.toString(f);
        Utils.uploadToS3(info, Utils.fmeasureOutput + Utils.countersFileName);
    }

    System.exit(result ? 0 : 1);
}
}
