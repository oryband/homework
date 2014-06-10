package com.dsp.ass2;

import java.io.IOException;
import java.util.Iterator;
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


public class LastDecade {

    private static final Logger logger = Utils.setLogger(Logger.getLogger(LastDecade.class.getName()));


    // Write { <w1,w2> : decade, w1, c(w1), c(w1,w2) }
    public static class MapClass extends Mapper<LongWritable, Text, DecadePmi, Text> {

        private DecadePmi decadePmi = new DecadePmi();
        private Text newValue = new Text();
        private String[] cDecades = new String[12];


        private static String calculatePMI(double cW1, double cW2, double cW1W2, double N) {
            return String.valueOf(Math.log(cW1W2) + Math.log(N) - Math.log(cW1) - Math.log(cW2));
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


    // Partition by 'decade + pmi + value' hash code.
    public static class PartitionerClass extends Partitioner<DecadePmi, Text> {
        @Override
        public int getPartition(DecadePmi key, Text value, int numPartitions) {
            // Calculate data's hash code, and bound by Integer maximum value,
            // then calculate the result(mod numPartition).
            Text data = new Text(key.decade + Utils.delim + key.PMI + Utils.delim + value.toString());
            return (data.hashCode() & Integer.MAX_VALUE) % numPartitions;
        }
    }


    // Write only pairs from decade == 200: That is, write { 200 pmi : w1, w2 }
    public static class ReduceClass extends Reducer<DecadePmi, Text, Text, Text> {

        private Text newKey = new Text();

        public void reduce(DecadePmi key, Iterable<Text> values, Context context)
            throws IOException, InterruptedException {

            Iterator<Text> it = values.iterator();

            while (it.hasNext() && Integer.parseInt(key.decade) == 200) {
                newKey.set(key.decade + Utils.delim + key.PMI);
                context.write(newKey, it.next());
            }
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        conf.set("mapred.reduce.tasks", Utils.reduceTasks);
        conf.set("mapred.reduce.slowstart.completed.maps", "1");

        Job job = new Job(conf, "LastDecade");

        job.setJarByClass(LastDecade.class);
        job.setMapperClass(MapClass.class);
        job.setPartitionerClass(PartitionerClass.class);
        job.setReducerClass(ReduceClass.class);
        job.setMapOutputKeyClass(DecadePmi.class);
        job.setMapOutputValueClass(Text.class);

        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        FileInputFormat.addInputPath(job, new Path(args[Utils.argInIndex]));
        FileOutputFormat.setOutputPath(job, new Path(args[Utils.argInIndex + 1]));

        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
