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


public class Sum {

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Sum.class.getName()));


    // Write { N1, N2, hypernym-index, un/related, dep-tree, i1, i2, : hits }
    public static class MapClass extends Mapper<LongWritable, Text, Text, LongWritable> {

        private Text newKey = new Text();
        private LongWritable newValue = new LongWritable();

        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            String v = value.toString();
            int i = v.lastIndexOf(Utils.delim);

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


    // Reducer/Combiner sums value longs.
    public static class ReduceClass extends Reducer<Text, LongWritable, Text, LongWritable> {
        private LongWritable newValue = new LongWritable();

        @Override
        public void reduce(Text key, Iterable<LongWritable> values, Context context)
            throws IOException, InterruptedException {

            newValue.set(Utils.sumValues(values));
            context.write(key, newValue);
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();

        Job job = new Job(conf, "Sum");

        job.setJarByClass(Sum.class);
        job.setMapperClass(MapClass.class);
        job.setPartitionerClass(PartitionerClass.class);
        job.setCombinerClass(ReduceClass.class);
        job.setReducerClass(ReduceClass.class);

        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        job.setMapOutputKeyClass(Text.class);
        job.setMapOutputValueClass(LongWritable.class);

        // Use this for local testing.
        // Add all but last argument as input path.
        for (int i=0; i < args.length -1; i++) {
            FileInputFormat.addInputPath(job, new Path(args[Utils.argInIndex +i]));
        }
        FileOutputFormat.setOutputPath(job, new Path(args[args.length -1]));

        // Use this for AWS.
        // NOTE we use two different input paths in here.
        // FileInputFormat.addInputPath(job, new Path(args[Utils.argInIndex]));
        // FileOutputFormat.setOutputPath(job, new Path(args[Utils.argInIndex +1]));

        boolean result = job.waitForCompletion(true);

        System.exit(result ? 0 : 1);
    }
}
