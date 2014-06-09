package com.dsp.ass2;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Counter;
import org.apache.hadoop.mapreduce.Counters;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Partitioner;
import org.apache.hadoop.mapreduce.Reducer;


public class FMeasure {

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Fmeasure.class.getName()));

    // NOTE we don't need to count TN.
    private static final Text tp = new Text("tp"),
            fp = new Text("tp"),
            fn = new Text("fn");


    // Recieves { decade, pmi : w1, w2 } and writes { tp/fp/fn : 1 }
    public static class MapClass extends Mapper<LongWritable, Text, Text, LongWritable> {

        private static final String unrelatedLink = Utils.s3Uri + "steps/FMeasure/input/wordsim-neg.txt",
                relatedLink = Utils.s3Uri + "steps/FMeasure/wordsim-pos.txt";

        private static final LongWritable one = new LongWritable(1);

        private static double threshold;

        // This structure will hold pairs of words with tag test result, according to learning samples:
        // { w1, w2 : tp/fp/fn }
        private static Map<String, Boolean> wordPairs = new HashMap <String, Boolean>();


        // Initialize tagged pairs structure. That is, filter untagged pairs (which we can't test on),
        // and order the remaining pairs lexicographically.
        private void initData(Boolean value, String pairs) {
            String[] splitPairs = pairs.split("\n"),
                words;

            for (int i=0; i < splitPairs.length; i++) {
                words = splitInfo[i].split(Utils.delim);

                if (words.length >= 2) {
                    // Arrange each pair lexicographically.
                    if (words[0].compareTo(words[1]) < 0) {
                        wordPairs.put(words[0] + words[1], value);
                    } else {
                        wordPairs.put(words[1] + words[0], value);
                    }
                }
            }
        }


        // Set threshold given as argument.
        @Override
        public void setup(Context context) {
            String t = context.getConfiguration().get("threshold", null);

            if (t == null) {
                logger.severe("Can't get threshold variable.");
                return;
            }

            threshold = Double.parseDouble(t);

            // Initialize word pairs results.
            initData(new Boolean(true), Utils.LinkToString(relatedLink));
            initData(new Boolean(false), Utils.LinkToString(unrelatedLink));
        }


        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            // Fetch words from value.
            String[] words = value.toString().split(Utils.delim);
            String decade = words[0],
                    w1 = words[2],
                    w2 = words[3];

            // Filter pairs not from last decade.
            if (!decade.equals("200")) {
                logger.severe("Unsupported decade: " + decade);
                return;
            }

            // we assume that w1 < w2
            Boolean testResult = wordPairs.get(w1 + w2);

            // Do nothing if pair isn't a learning sample.
            if (testResult == null) {
                return;
            }

            // Fetch pmi from key.
            double PMI = Double.parseDouble(words[1]);

            // Write pair test result (tp/fp/fn).
            if (testResult.booleanValue()) {
                if (PMI >= threshold) {
                    context.write(tp, one);  // TP
                } else {
                    context.write(fn, one);  // FN
                }
            } else {
                if (PMI >= threshold) {
                    context.write(fp, one);  // FP
                }
            }
        }
    }


    // Sum every identical count values before sending to reducer.
    public static class CombineClass extends Reducer<Text, LongWritable, Text, LongWritable> {
        @Override
        public void reduce(Text key, Iterable<LongWritable> values, Context context) throws IOException,  InterruptedException {
            context.write(key, new LongWritable(Utils.sumValues(values)));
        }
    }


    // Partition by test result (tp/tn/fp).
    public static class PartitionerClass extends Partitioner<Text, LongWritable> {
        @Override
        public int getPartition(Text key, LongWritable value, int numPartitions) {
            return (getPartitionNumber(key) % numPartitions);
        }


        private int getPartitionNumber(Text key) {
            if (key.equals(tp)) {
                return 0;
            } else if(key.equals(fp)) {
                return 1;
            } else {  // this means key.equals(fn)
                return 2;
            }
        }
    }


    public static class ReduceClass extends Reducer<Text, LongWritable, Text, LongWritable> {

        public static enum COUNTER {
            tp, fp , fn
        };


        // Updates global test result counters.
        private void updateCounter(Text key, long sum, Context context) {
            Counter counter = null;

            if (key.equals(tp)) {
                counter = context.getCounter(COUNTER.tp);
            } else if(key.equals(fp)) {
                counter = context.getCounter(COUNTER.fp);
            } else if (key.equals(fn)){
                counter = context.getCounter(COUNTER.fn);
            }

            if (counter != null) {
                counter.increment(sum);
            }
        }


        // Update tp/fp/fn global variables (will be uploaded to S3 in main()):
        // Write { tp/fp/fn : num of tp/fp/fn accoridngly. }
        public void reduce(Text key, Iterable<LongWritable> values, Context context)
                throws IOException, InterruptedException {

            long sum = Utils.sumValues(values);
            context.write(key, new LongWritable(sum));
            updateCounter(key, sum, context);
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
