package com.dsp.ass2;

import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.Partitioner;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.io.LongWritable;


public class AfterCount {

    public static class MapClass extends Mapper<LongWritable, Text, Text, Text> {
        private Text word = new Text();

        @Override
        public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
            String w1, w2, countw1 , countw1w2;
            Text newValue;

            String[] arr = value.toString().split("\t");
            w1 = arr[0];
            w2 = arr[1];
            countw1w2 = arr[2];
            countw1 = arr[3];
            newValue = new Text(countw1w2 + "\t" + w1 +"\t" + countw1);

            word.set(w1 + "\t" + w2);
            context.write(word, newValue);

            word.set(w2 + "\t" + w1);
            context.write(word, newValue);
        }
    }


    public static class ReduceClass extends Reducer<Text,Text,Text,Text> {

        @Override

        public void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {

            String w1, w2, countw1 = "0", countw2 = "0", countw1w2 = "0";
            Text newKey, newValue;

            String[] words = key.toString().split("\t");
            w1 = words[0];
            w2 = words[1];

            for (Text value : values) {

                String[] split = value.toString().split("\t");
                countw1w2 = split[0];

                if (split[1].equals(w1)) {
                    countw1 = split[2];
                }

                else {
                    countw2 = split[2];
                }
            }

            newKey = new Text(w1 + "\t" + w2);
            newValue = new Text(countw1w2 + "\t" + countw1 + "\t" + countw2);
            context.write(newKey, newValue);
        }
    }


    public static class PartitionerClass extends Partitioner<Text, Text> {
        @Override
        public int getPartition(Text key, Text value, int numPartitions) {
            return getLanguage(key) % numPartitions;
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
        //conf.set("mapred.map.tasks","10");
        //conf.set("mapred.reduce.tasks","2");
        Job job = new Job(conf, "word count");
        job.setJarByClass(AfterCount.class);
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
