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


public class Join {

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Count.class.getName()));


    public static class MapClass extends Mapper<LongWritable, Text, Text, Text> {

        private Text newKey = new Text();
        private Text newValue = new Text();

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
                   cW1W2 = words[4];

            newValue.set(w1 + Utils.delim + cW1 + Utils.delim + cW1W2);

            newKey.set(decade + Utils.delim + w1 + Utils.delim + w2);
            context.write(newKey, newValue);

            newKey.set(decade + Utils.delim + w2 + Utils.delim + w1);
            context.write(newKey, newValue);
        }
    }


    // Partition by 'decade + w1 + w2' hash code.
    public static class PartitionerClass extends Partitioner<Text, Text> {
        @Override
        public int getPartition(Text key, Text value, int numPartitions) {
            return (key.hashCode() & Integer.MAX_VALUE) % numPartitions;
        }

    }


    public static class ReduceClass extends Reducer<Text,Text,Text,Text> {

        private Text newValue = new Text();

        // For every <w1,w2> - Write { <decade, w1 ,w2> : c(w1), c(w2), c(w1,w2) }
        @Override
        public void reduce(Text key, Iterable<Text> values, Context context)
            throws IOException, InterruptedException {
            String cW1 = "0", cW2 = "0", cW1W2 = "0";

            // Fetch w1, w2, c(w1), c(w2), c(w1,w2).
            String[] parseKey = key.toString().split(Utils.delim);
            String w1 = parseKey[1];

            String[] counters;
            for (Text value : values) {
                counters = value.toString().split(Utils.delim);

                if (counters[0].equals(w1)) {
                    cW1 = counters[1];
                    cW1W2 = counters[2];  // TODO Should we sum cW1W2 + cW2W1 ? Does the order of words matter?
                } else {
                    cW2 = counters[1];
                }
            }

            newValue.set(cW1 + Utils.delim + cW2 + Utils.delim + cW1W2);
            context.write(key, newValue);
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        conf.set("mapred.reduce.slowstart.completed.maps", "1");
        //conf.set("mapred.map.tasks", "10");
        //conf.set("mapred.reduce.tasks", "2");

        Job job = new Job(conf, "Join");

        job.setJarByClass(Join.class);
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
