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

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Count.class.getName()));


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
                cDecades[i] = context.getConfiguration().get("N_" + (i + 190), "-1");
            }
        }


        // Write { <w1,w2> : decade, w1, c(w1), c(w1,w2) }
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
                   cDecade = cDecades[Integer.parseInt(decade) - 190],
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
            newValue.set(decade + Utils.delim + w1 + Utils.delim + w2);
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

            int decadeIndex = Integer.parseInt(decade) - 190;
            if (pairsPerDecade[decadeIndex] -- > 0) {
                newKey.set(key.decade + Utils.delim + key.PMI);
                context.write(newKey, values.iterator().next());
            }
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        //conf.set("mapred.map.tasks", "10");
        //conf.set("mapred.reduce.tasks", "2");

        conf.set("mapred.reduce.slowstart.completed.maps", "1");
        conf.set("pairsPerDecade", args[2]);  // TODO Change args index for amazon.

        conf.set("N_199", "9559");
        conf.set("N_200", "23823");

        Job job = new Job(conf, "Join");

        job.setJarByClass(Calculate.class);
        job.setMapperClass(MapClass.class);
        job.setPartitionerClass(PartitionerClass.class);

        // job.setCombinerClass(CombineClass.class);
        job.setReducerClass(ReduceClass.class);
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);
        FileInputFormat.addInputPath(job, new Path(args[0]));
        FileOutputFormat.setOutputPath(job, new Path(args[1]));

        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
