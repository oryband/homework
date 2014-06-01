package com.dsp.ass2;

import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.Partitioner;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.io.LongWritable;


public class Count {

    private final static String ngramDelim = " ",
            wordHeader = "*";

    // Sum all members in list.
    private static int sumValues(Iterable<IntWritable> values) {
        int sum = 0;
        for (IntWritable value : values) {
            sum += value.get();
        }
        return sum;
    }


    public static class MapClass extends Mapper<LongWritable, Text, Text, IntWritable> {

        private final IntWritable one = new IntWritable(1);
        private Text word = new Text();

        // Emit 5 times for each 5-gram:
        // 1 time for <w,c(w)> , emitted as { <w,*> : c(w) }
        // 4 times for each <<w,wi>, c(w,wi)> s.t. i=1..4 , emitted as { <w,wi> : c(w), c(w,wi) }
        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            // Split n-gram line into words, and handle <w,wi> pairs.
            String[] words = value.toString().split(ngramDelim);
            int center, i;
            if (words.length > 0) {
                center = words.length / 2;  // Main (central) word index.

                word.set(words[center] + Utils.delim + wordHeader);
                context.write(word, one);

                for (i=0 ; i < words.length; i++) {
                    if (i != center) {
                        word.set(words[center] + Utils.delim + words[i]);
                        context.write(word, one);
                    }
                }
            }
        }
    }


    public static class CombineClass extends Reducer<Text,IntWritable,Text,IntWritable> {

        private IntWritable sumWrt = new IntWritable();

        // Sum every identical count values before sending to reducer.
        @Override
        public void reduce(Text key, Iterable<IntWritable> values, Context context)
            throws IOException, InterruptedException {
            sumWrt.set(sumValues(values));
            context.write(key, sumWrt);
        }
    }


    public static class ReduceClass extends Reducer<Text,IntWritable,Text,Text> {

        private int cw;  // c(w)

        // If key is <w,*>: Write { <w,*> : c(w) }
        // Else key is <w,wi>: Write { <w,wi> : c(w), c(w,wi) }
        @Override
        public void reduce(Text key, Iterable<IntWritable> values, Context context)
            throws IOException, InterruptedException {

            int sum = sumValues(values);
            if (key.toString().split(Utils.delim)[1].equals(wordHeader)) {
                cw = sum;
            } else {
                String val = Integer.toString(cw) + Utils.delim + Integer.toString(sum);
                context.write(key, new Text(val));
            }
        }
    }


    public static class PartitionerClass extends Partitioner<Text, IntWritable> {
        // TODO Do this in a smarter way.
        @Override
        public int getPartition(Text key, IntWritable value, int numPartitions) {
            return getLanguage(new Text(
                        key.toString().split(Utils.delim)[0])) % numPartitions;
        }

        private int getLanguage(Text key) {
            if (key.getLength() > 0) {
                int c = key.charAt(0);
                if (c >= Long.decode("0x05D0").longValue() && c <= Long.decode("0x05EA").longValue())
                    return 1;
            }
            return 0;
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        conf.set("mapred.reduce.slowstart.completed.maps", "1");
        //conf.set("mapred.map.tasks","10");
        //conf.set("mapred.reduce.tasks","2");
        Job job = new Job(conf, "Count");

        job.setJarByClass(Count.class);
        job.setMapperClass(MapClass.class);
        job.setPartitionerClass(PartitionerClass.class);
        job.setCombinerClass(CombineClass.class);
        job.setReducerClass(ReduceClass.class);

        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(IntWritable.class);

        // TODO change args to 1,2 when testing on amazon ecr.
        FileInputFormat.addInputPath(job, new Path(args[0]));
        FileOutputFormat.setOutputPath(job, new Path(args[1]));

        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
