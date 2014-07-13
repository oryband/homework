package com.dsp.ass3;

import java.io.IOException;
import java.util.logging.Logger;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Partitioner;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;


public class Headers {
    private static String attributeHeader = "@ATTRIBUTE",
            attributeType = "INTEGER";

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Headers.class.getName()));


    // Mapper just passes on data.
    public static class MapClass extends Mapper<LongWritable, Text, Text, NullWritable> {


        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            context.write(value, NullWritable.get());
        }
    }


    // Partition by key hash code.
    public static class PartitionerClass extends Partitioner<Text, NullWritable> {
        @Override
        public int getPartition(Text key, NullWritable value, int numPartitions) {
            return (key.hashCode() & Integer.MAX_VALUE) % numPartitions;
        }
    }


    // Write: @ATTRIBUTE 'dep-tree', i1, i2 INTEGER for every vector.
    public static class ReduceClass extends Reducer<Text, NullWritable, Text, NullWritable> {

        private Text newKey = new Text();


        @Override
        public void reduce(Text key, Iterable<NullWritable> values, Context context)
            throws IOException, InterruptedException {

            newKey.set(attributeHeader
                    + Utils.delim + weka.core.Utils.quote(key.toString())
                    + Utils.delim + attributeType);

            context.write(newKey, NullWritable.get());
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();

        Job job = new Job(conf, "Headers");

        job.setJarByClass(Headers.class);
        job.setMapperClass(MapClass.class);
        job.setPartitionerClass(PartitionerClass.class);
        job.setReducerClass(ReduceClass.class);

        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(NullWritable.class);

        job.setMapOutputKeyClass(Text.class);
        job.setMapOutputValueClass(NullWritable.class);

        FileInputFormat.addInputPath(job, new Path(args[Utils.argInIndex]));
        FileOutputFormat.setOutputPath(job, new Path(args[Utils.argInIndex +1]));

        boolean result = job.waitForCompletion(true);

        System.exit(result ? 0 : 1);
    }
}
