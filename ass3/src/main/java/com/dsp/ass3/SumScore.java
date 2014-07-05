package com.dsp.ass3;

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


public class SumScore {

    private static final Logger logger = Utils.setLogger(Logger.getLogger(SumScore.class.getName()));


    // Mapper just passes on data.
    public static class MapClass extends Mapper<LongWritable, Text, Text, LongWritable> {

        private Text newKey = new Text();
        private LongWritable newValue = new LongWritable();

        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            // Fetch key/value and emit them.
            String v = value.toString();
            int i = v.lastIndexOf(Utils.keyValueDelim);

            newKey.set(v.substring(0, i));
            newValue.set(Long.parseLong(v.substring(i+1)));

            context.write(newKey, newValue);
        }
    }


    // Partition by key hash code.
    public static class PartitionerClass extends Partitioner<Text, LongWritable> {
        @Override
        public int getPartition(Text key, LongWritable value, int numPartitions) {
            return (key.hashCode() & Integer.MAX_VALUE) % numPartitions;
        }
    }


    // Sum score for each dep-tree and write { dep-tree, hypernym-index, hyponym-index : score }
    public static class ReduceClass extends Reducer<Text, LongWritable, Text, LongWritable> {

        private LongWritable newValue = new LongWritable();
        private int DpMin;

        // Init DPMin argument.
        @Override
        public void setup(Context context) {
            DpMin = Integer.parseInt(context.getConfiguration().get(Utils.DpMinArg, "-1"));
            if (DpMin == -1) {
                logger.severe("Failed fetching DPMin argument.");
                return;
            }
        }


        @Override
        public void reduce(Text key, Iterable<LongWritable> values, Context context)
            throws IOException, InterruptedException {

            // Calculate values length.
            long l = 0,
                 sum = 0;
            for (LongWritable value : values) {
                l++;
                sum += value.get();
            }

            // Ignore dep-tree if it doesn't match to enough distinct noun-pairs.
            if (l < DpMin) {
                return;
            }

            // Set value to sum of values.
            newValue.set(sum);

            context.write(key, newValue);
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();

        conf.set(Utils.DpMinArg, args[Utils.argInIndex + 2]);

        Job job = new Job(conf, "SumScore");

        job.setJarByClass(Pairs.class);
        job.setMapperClass(MapClass.class);
        job.setPartitionerClass(PartitionerClass.class);
        job.setReducerClass(ReduceClass.class);

        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(LongWritable.class);

        FileInputFormat.addInputPath(job, new Path(args[Utils.argInIndex]));
        FileOutputFormat.setOutputPath(job, new Path(args[Utils.argInIndex + 1]));

        boolean result = job.waitForCompletion(true);

        System.exit(result ? 0 : 1);
    }
}
