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


    public static class MapClass extends Mapper<LongWritable, Text, CenturyPmi, Text> {

        private CenturyPmi newKey = new CenturyPmi();
        private Text newValue = new Text();


        private static String calculatePMI(double cW1, double cW2, double cW1W2, double N) {
            double answer = Math.log(cW1W2) + Math.log(N) - Math.log(cW1) - Math.log(cW2);
            return String.valueOf(answer);
        }


        // Write { <w1,w2> : century , w1, c(w1), c(w1,w2) }
        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            // Fetch words from value.
            String[] words = value.toString().split(Utils.delim);
            String century = words[0],
                    w1 = words[1],
                    w2 = words[2],
                    cW1 = words[3],
                    cW2 = words[4],
                    cW1W2 = words[5],
                    N = context.getConfiguration().get("N_" + century, "-1"), // TODO Move this to setup, avoid redundany.
                    pmi;

            if (N.equals("-1")) {
                logger.severe("Unsupported century: " + century);
                return;
            }

            pmi = calculatePMI(
                    Double.parseDouble(cW1),
                    Double.parseDouble(cW2),
                    Double.parseDouble(cW1W2),
                    Double.parseDouble(N));

            newKey.set(century, pmi);
            newValue.set(w1 + Utils.delim + w2);
            context.write(newKey, newValue);
        }
    }


    // Partition by century. That is, each reducer receives keys with a single century.
    // If there are more reducers then centuries, then these are unused.
    public static class PartitionerClass extends Partitioner<CenturyPmi, Text> {
        @Override
        public int getPartition(CenturyPmi key, Text value, int numPartitions) {
            return Integer.parseInt(key.century) % numPartitions;
        }
    }


    public static class ReduceClass extends Reducer<CenturyPmi,Text,Text,Text> {

        private Text newKey = new Text();
        private int[] pairsPerCentury = new int[12];  // Counter for remaining pairs for each century to write.


        // Init counter for remaining pairs for each century.
        @Override
        public void setup(Context context) {
            int pairsNum = Integer.parseInt(context.getConfiguration().get("pairsPerCentury", "-1"));
            for (int i=0; i < 12; i++) {
                pairsPerCentury[i] = pairsNum;
            }
        }


        public void reduce(CenturyPmi key, Iterable<Text> values, Context context)
            throws IOException, InterruptedException {

            String century = key.century;

            int centuryIndex = Integer.parseInt(century) - 190;
            if (pairsPerCentury[centuryIndex] -- > 0) {
                newKey.set(key.century + Utils.delim + key.PMI);
                context.write(newKey, values.iterator().next());
            }
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        conf.set("mapred.reduce.slowstart.completed.maps", "1");
        //conf.set("mapred.map.tasks", "10");
        //conf.set("mapred.reduce.tasks", "2");

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
