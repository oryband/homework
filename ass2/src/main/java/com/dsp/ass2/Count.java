package com.dsp.ass2;

import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Counters;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.Partitioner;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.SequenceFileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.io.LongWritable;



public class Count {

    private final static String ngramDelim = "\t",
            wordsDelim = " ",
            wordHeader = "*";

    private final static int minCentury = 199;

    // Sum all members in list.
    private static int sumValues(Iterable<IntWritable> values) {
        int sum = 0;
        for (IntWritable value : values) {
            sum += value.get();
        }
        return sum;
    }


    public static class MapClass extends Mapper<LongWritable, Text, Text, IntWritable> {

        private IntWritable num = new IntWritable();
        private Text word = new Text();

        // For every word `w` in n-gram: emit { century, w, * : c(w) }
        // For every central word `w` in n-gram: emit { century, w, wi : c(w,wi) } , i=1..4 (its neithbours)
        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            // Split n-gram into words, and handle <w,wi> pairs.
            String[] ngram = value.toString().split(ngramDelim),
                words = ngram[0].split(wordsDelim);

            int century = Integer.parseInt(ngram[1]) / 10,
                occurences = Integer.parseInt(ngram[2]);
            int center, i;
            if (words.length > 0 && century >= minCentury) {
                center = words.length / 2;  // Main (central) word index.

                for (i=0; i < words.length; i++) {
                    // Emit for every word in n-gram.
                    num.set(occurences);

                    word.set(century + Utils.delim + words[i] + Utils.delim + wordHeader);
                    context.write(word, num);

                    // Emit for every central word.
                    if (i != center) {
                        word.set(century + Utils.delim + words[center] + Utils.delim + words[i]);
                        context.write(word, num);
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


    public static class PartitionerClass extends Partitioner<Text, IntWritable> {
        @Override
        // Partition by 'century + w' hash code.
        public int getPartition(Text key, IntWritable value, int numPartitions) {
            String[] words = key.toString().split(Utils.delim);
            Text data = new Text(words[0] + Utils.delim + words[1]);
            // Calculate data's hash code, and bound by Integer maximum value,
            // then calculate the result(mod numPartition).
            return data.hashCode() & Integer.MAX_VALUE % numPartitions;
        }
    }


    public static class ReduceClass extends Reducer<Text,IntWritable,Text,Text> {

        // Corpus word counter by century.
        public static enum N_COUNTER {
            N_190,  // 1900
            N_191,  // 1910
            N_192,
            N_193,
            N_194,
            N_195,
            N_196,
            N_197,
            N_198,
            N_199,
            N_200,
            N_201;  // 2010
        };

        private int cw;  // c(w)

        // If key is 'century, w, *' Write { century, w, * : c(w) }
        // Else key is <w,wi>: Write { <w,wi> : c(w), c(w,wi) }
        @Override
        public void reduce(Text key, Iterable<IntWritable> values, Context context)
            throws IOException, InterruptedException {

            int sum = sumValues(values);
            if (key.toString().split(Utils.delim)[2].equals(wordHeader)) {
                // 'century, w,*' case:
                cw = sum;

                // TODO sort by decades
                // Increment global word counter per decade.
                context.getCounter(N_COUNTER.N).increment(cw);
            } else {
                String val = Integer.toString(cw) + Utils.delim + Integer.toString(sum);
                context.write(key, new Text(val));
            }
        }
    }


    // Google N-Gram reader.
    public static class MySequenceFileInputFormat extends SequenceFileInputFormat<LongWritable,Text> {}


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        conf.set("mapred.reduce.slowstart.completed.maps", "1");
        // conf.set("pairsPerDecade", args[2]);  // TODO change from 2 to 3 for amazon.
        // conf.set("mapred.map.tasks","10");
        // conf.set("mapred.reduce.tasks","2");

        Job job = new Job(conf, "Join");

        // Read from Google N-Gram.
        // TODO try to use original SequenceFileInputFormat.class.
        job.setInputFormatClass(MySequenceFileInputFormat.class);

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

        boolean result = job.waitForCompletion(true);

        // if (result) {
        //     Counters counters = job.getCounters();
        //     long nCounter = counters.findCounter(ReduceClass.N_COUNTER.N).getValue();
        //     System.out.println(nCounter);
        //     conf.set("n", Long.toString(nCounter));
        // }

        System.exit(result ? 0 : 1);
    }
}
