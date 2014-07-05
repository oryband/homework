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

import com.dsp.ass2.Utils;


public class Sum {

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Sum.class.getName()));

    public static class MapClass extends Mapper<LongWritable, Text, Text, Text> {

        private Text newKey = new Text(),
                newValue = new Text();

        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            // Fetch pairs and tag from value.
            String[] words = value.toString().split(Utils.keyValueDelim);
            newKey.set(words[0]);
            newValue.set(words[1]);

            context.write(newKey, newValue);
        }
    }


    // Partition by key hash code.
    public static class PartitionerClass extends Partitioner<Text, Text> {
        @Override
        public int getPartition(Text key, Text value, int numPartitions) {
            return (key.hashCode() & Integer.MAX_VALUE) % numPartitions;
        }

    }


    // Reducer just passes on data.
    public static class ReduceClass extends Reducer<Text, Text, Text, Text> {

        private Text newValue = new Text();
        private int DPMin = 0;

        @Override
        public void setup(Context context) {
            context.getConfiguration().get("DPMin", "-1");
        }

        @Override
        public void reduce(Text key, Iterable<Text> values, Context context)
            throws IOException, InterruptedException {

            context.write(key, values.iterator().next());
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();

        Job job = new Job(conf, "Sum");

        job.setJarByClass(Pairs.class);
        job.setMapperClass(MapClass.class);
        job.setPartitionerClass(PartitionerClass.class);
        job.setReducerClass(ReduceClass.class);

        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        FileInputFormat.addInputPath(job, new Path(args[Utils.argInIndex]));
        FileOutputFormat.setOutputPath(job, new Path(args[Utils.argInIndex + 1]));

        boolean result = job.waitForCompletion(true);

        System.exit(result ? 0 : 1);
    }
}
