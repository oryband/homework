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

    public static class MapClass extends Mapper<LongWritable, Text, Text, IntWritable> {

        private final static IntWritable one = new IntWritable(1);
        private Text word = new Text();

        // map function emit 5 times for each 5-gram : 4 times for the center word with the other words,
        // and one time (with '*' as word) to calculate the instance of the word itself.
        @Override
        public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
            String[] arr = value.toString().split(" ");

            switch (arr.length) {

                case 1:  //  just counting to word.
                    word.set(arr[0] + "\t*");
                    context.write(word, one);

                    break;

                case 2:  // consider the pair.
                    word.set(arr[0] + "\t*");
                    context.write(word, one);

                    word.set(arr[0] + "\t" + arr[1]);
                    context.write(word, one);

                    break;

                case 3: // choose the middle word and create its context
                    word.set(arr[1] + "\t*");
                    context.write(word, one);

                    for (int i = 0 ; i < 3 ; i++) {
                        if (i != 1) {
                            word.set(arr[1] + "\t" + arr[i]);
                            context.write(word, one);
                        }
                    }

                    break;

                case 4: // choose the third word and create its context

                    word.set(arr[2] + "\t*");
                    context.write(word, one);

                    for (int i = 0 ; i < 4 ; i++) {
                        if (i != 2) {
                            word.set(arr[2] + "\t" + arr[i]);
                            context.write(word, one);
                        }
                    }

                    break;

                case 5:
                    word.set(arr[2] + "\t*");
                    context.write(word, one);

                    for (int i = 0 ; i < 5 ; i++) {
                        if (i != 2) {
                            word.set(arr[2] + "\t" + arr[i]);
                            context.write(word, one);
                        }
                    }

                    break;

                default:
                    break;
            }
        }
    }


    // Combiner class just summing to values before sending it.
    public static class CombineClass extends Reducer<Text,IntWritable,Text,IntWritable> {
        @Override
        public void reduce(Text key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException {
            int sum = 0;
            for (IntWritable value : values) {
                sum += value.get();
            }
            context.write(key, new IntWritable(sum));
        }
    }



    public static class ReduceClass extends Reducer<Text,IntWritable,Text,Text> {
        private int w1sum = 0;

        /* public void setup(Context context){
        Configuration conf = context.getConfiguration();
        JobConf jobConf = new JobConf(conf);
        JobClient client;
        try {
            jobID = jobConf.get("mapred.job.id");
            System.out.println(jobID);
            client = new JobClient(jobConf);
            RunningJob job = client.getJob(JobID.forName(jobID));

            if (job == null) {
                System.out.println("No job with ID found " + jobID);
            } else {
                Counters counters = job.getCounters();
                N = counters.getCounter(MapClass.Counter.count);
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    } */

        @Override
        public void reduce(Text key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException {

            int sum = 0;
            for (IntWritable value : values) {
                sum += value.get();
            }

            if (key.toString().split("\t")[1].equals("*")) {
                w1sum = sum;
            }

            else {
                String val = Integer.toString(sum) + "\t" + Integer.toString(w1sum);
                context.write(key, new Text(val));
            }
        }
    }


    public static class PartitionerClass extends Partitioner<Text, IntWritable> {
        @Override
        public int getPartition(Text key, IntWritable value, int numPartitions) {
            return getLanguage(new Text(key.toString().split("\t")[0])) % numPartitions;
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
        job.setJarByClass(Count.class);
        job.setMapperClass(MapClass.class);
        job.setPartitionerClass(PartitionerClass.class);
        job.setCombinerClass(CombineClass.class);
        job.setReducerClass(ReduceClass.class);
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(IntWritable.class);
        FileInputFormat.addInputPath(job, new Path(args[0]));
        FileOutputFormat.setOutputPath(job, new Path(args[1]));
        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
