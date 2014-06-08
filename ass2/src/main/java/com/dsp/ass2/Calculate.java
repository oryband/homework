package com.dsp.ass2;

import java.io.IOException;
import java.util.logging.Logger;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Partitioner;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;


public class Calculate {

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Calculate.class.getName()));

    private static long totalRecords = 0;


    // Write { <w1,w2> : decade, w1, c(w1), c(w1,w2) }
    public static class MapClass extends Mapper<LongWritable, Text, DecadePmi, Text> {

        private DecadePmi decadePmi = new DecadePmi();
        private Text newValue = new Text();
        private String[] cDecades = new String[12];


        private static String calculatePMI(double cW1, double cW2, double cW1W2, double N) {
            double answer = Math.log(cW1W2) + Math.log(N) - Math.log(cW1) - Math.log(cW2);
            return String.valueOf(answer);
        }


        // Set decade counters.
        @Override
        public void setup(Context context) {
            for (int i=0; i < cDecades.length; i++) {
                cDecades[i] = context.getConfiguration().get("N_" + (i + Utils.minDecade), "-1");
            }
        }


        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            // Fetch words from value.
            String[] words = value.toString().split(Utils.delim);
            String decade = words[0],
                   w1 = words[1],
                   w2 = words[2],
                   cW1 = words[3],
                   cW2 = words[4],
                   cW1W2 = words[5],
                   cDecade = cDecades[Integer.parseInt(decade) - Utils.minDecade],
                   pmi;

            if (cDecade.equals("-1")) {
                logger.severe("Unsupported decade: " + decade);
                return;
            }

            pmi = calculatePMI(
                    Double.parseDouble(cW1),
                    Double.parseDouble(cW2),
                    Double.parseDouble(cW1W2),
                    Double.parseDouble(cDecade));

            decadePmi.set(decade, pmi);
            newValue.set(w1 + Utils.delim + w2);
            context.write(decadePmi, newValue);
        }
    }


    // Partition by decade. That is, each reducer receives keys from a single decade.
    // If there are more reducers then decades, then these are unused.
    public static class PartitionerClass extends Partitioner<DecadePmi, Text> {
        @Override
        public int getPartition(DecadePmi key, Text value, int numPartitions) {
            return Integer.parseInt(key.decade) % numPartitions;
        }
    }


    public static class ReduceClass extends Reducer<DecadePmi, Text, Text, Text> {

        private Text newKey = new Text();
        private int[] pairsPerDecade = new int[12];  // Counter for remaining pairs for each decade to write.


        // Init counter for remaining pairs for each decade.
        @Override
        public void setup(Context context) {
            int pairsNum = Integer.parseInt(context.getConfiguration().get("pairsPerDecade", "-1"));
            for (int i=0; i < 12; i++) {
                pairsPerDecade[i] = pairsNum;
            }
        }


        public void reduce(DecadePmi key, Iterable<Text> values, Context context)
                throws IOException, InterruptedException {

            String decade = key.decade;

            int decadeIndex = Integer.parseInt(decade) - Utils.minDecade;
            if (pairsPerDecade[decadeIndex] -- > 0) {
                newKey.set(key.decade + Utils.delim + key.PMI);
                context.write(newKey, values.iterator().next());
            }
        }
    }


    // Updated decade and total-records counters, using data from previous steps.
    private static void updateCounters(Configuration conf) {
        String[] splitFile, splitRow;
        String info;

        // Read Count step output.
        info = Utils.LinkToString(Utils.s3Uri + Utils.countOutput + Utils.countersFileName);
        if (info == null) {
            logger.severe("Error opening Count output file: " + Utils.s3Uri + Utils.countOutput);
            return;
        }

        // Read Count step decade and total-records counters.
        splitFile = info.split("\n");

        for (int i = 0; i < splitFile.length; i++) {
            splitRow = splitFile[i].split("\t");

            if (splitRow[0].equals("counters")) {
                for (int j = 1; j < splitRow.length; j++ ) {
                    conf.set("N_" + (j + Utils.minDecade - 1), splitRow[j]);
                }
            } else if (splitRow[0].equals("totalrecords")) {
                totalRecords = Long.parseLong(splitRow[1]);
            }
        }

        // Read Join step output.
        info = Utils.LinkToString(Utils.s3Uri + Utils.joinOutput + Utils.countersFileName);
        if (info == null) {
            logger.severe("Error opening Count output file: " + Utils.s3Uri + Utils.joinOutput);
            return;
        }

        // Read Join step total-records counters (no decades are being count at this step).
        splitFile = info.split("\n");

        for (int i = 0; i < splitFile.length; i++) {
            splitRow = splitFile[i].split("\t");
            if (splitRow[0].equals("totalrecords")) {
                totalRecords += Long.parseLong(splitRow[1]);
            }
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();

        // conf.set("mapred.map.tasks", Utils.mapTasks);
        // conf.set("mapred.reduce.tasks", Utils.reduceTasks);

        conf.set("mapred.reduce.slowstart.completed.maps", "1");
        conf.set("pairsPerDecade", args[Utils.argInIndex + 2]);

        updateCounters(conf);

        Job job = new Job(conf, "Join");

        job.setJarByClass(Calculate.class);
        job.setMapperClass(MapClass.class);
        job.setPartitionerClass(PartitionerClass.class);

        job.setReducerClass(ReduceClass.class);
        job.setMapOutputKeyClass(DecadePmi.class);
        job.setMapOutputValueClass(Text.class);
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        FileInputFormat.addInputPath(job, new Path(args[Utils.argInIndex]));
        FileOutputFormat.setOutputPath(job, new Path(args[Utils.argInIndex + 1]));

        boolean result = job.waitForCompletion(true);

        // Get totalRecords counter from Count & Join steps,
        // add this step (Calculate) records to it,
        // and upload final counters to S3.
        if (result) {
            long calculateTotalRecords = job.getCounters()
                .findCounter("org.apache.hadoop.mapred.Task$Counter", "MAP_OUTPUT_RECORDS").getValue();
            String info = "totalrecords\t" + Long.toString(totalRecords + calculateTotalRecords);
            Utils.uploadToS3(info, Utils.calculateOutput + Utils.countersFileName);
        }

        System.exit(result ? 0 : 1);
    }
}
