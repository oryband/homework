package com.dsp.ass2;

import java.io.IOException;
import java.io.StringReader;
import java.util.logging.Logger;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Counter;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Partitioner;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.SequenceFileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.core.StopFilter;
import org.apache.lucene.analysis.en.EnglishAnalyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.analysis.standard.StandardTokenizer;
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute;
import org.apache.lucene.analysis.util.CharArraySet;
import org.apache.lucene.util.Version;


public class Count {

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Count.class.getName()));

    private static final String ngramDelim = "\t",
            wordsDelim = " ",
            wordHeader = "*";

    private static final int minCentury = 199;


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


        // TODO This should replace stop words with '*' instead of removing them completely. It screws up c(w,wi).
        // Returns the same string with stop words & punctuation removed. removed.
        public String removeStopWords(String words) {
            CharArraySet stopWords = EnglishAnalyzer.getDefaultStopSet();
            // CharArraySet stopWords = StandardAnalyzer.STOP_WORDS_SET;
            TokenStream tokenStream = new StandardTokenizer(Version.LUCENE_48, new StringReader(words.trim()));
            tokenStream = new StopFilter(Version.LUCENE_48, tokenStream, stopWords);
            CharTermAttribute charTermAttribute = tokenStream.addAttribute(CharTermAttribute.class);
            StringBuilder sb = new StringBuilder();
            String output;

            try {
                // Append only non stop words.
                tokenStream.reset();
                while (tokenStream.incrementToken()) {
                    String term = charTermAttribute.toString();
                    sb.append(term + " ");
                }

                output = sb.toString().trim();  // Clean trailing whitespace.
                tokenStream.close();
            } catch (IOException e) {
                // On any error, return the original n-gram (and notify user).
                logger.severe("Error filtering stop words from n-gram: " + words);
                return words;
            }

            return output;
        }


        // For every word `w` in n-gram: emit { century, w, * : c(w) }
        // For every central word `w` in n-gram: emit { century, w, wi : c(w,wi) } , i=1..4 (its neithbours)
        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            // Convert n-gram to lowercase, Split into words, and handle <w,wi> pairs.
            String[] ngram = value.toString().toLowerCase().split(ngramDelim),
                words = ngram[0].split(wordsDelim);

            String centralWord;

            int century = Integer.parseInt(ngram[1]) / 10,
                occurences = Integer.parseInt(ngram[2]),
                center,
                i;

            if (words.length > 0 && century >= minCentury) {
                // Get central word in n-gram.
                center = words.length / 2;
                centralWord = words[center];

                // Remove stop words, count c(w) for every word in pair,
                // and count pairs only if central word wasn't filtered.
                // That is - it wasn't a stop word.
                words = removeStopWords(ngram[0]).split(wordsDelim);

                if (words.length > 0) {
                    center = words.length / 2;

                    boolean countPairs = false;

                    // Count pairs if central word in n-gram was not a stop word.
                    if (centralWord.equals(words[center])) {
                        centralWord = words[center];
                        countPairs = true;
                    }

                    for (i=0; i < words.length; i++) {
                        // Emit for every word in n-gram.
                        num.set(occurences);

                        // Emit c(w) for every word.
                        word.set(century + Utils.delim + words[i] + Utils.delim + wordHeader);
                        context.write(word, num);

                        // Emit c(w,wi) for central word, if central wors wasn't a stop word.
                        if (countPairs && i != center) {
                            word.set(century + Utils.delim + words[center] + Utils.delim + words[i]);
                            context.write(word, num);
                        }
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
            return (data.hashCode() & Integer.MAX_VALUE) % numPartitions;
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

            String[] words = key.toString().split(Utils.delim);
            String century = words[0], wi = words[2];
            int sum = sumValues(values);

            if (wi.equals(wordHeader)) {
                // 'century, w,*' case:
                cw = sum;
                updateCounter(century, context);

            } else {
                String val = Integer.toString(cw) + Utils.delim + Integer.toString(sum);
                context.write(key, new Text(val));
            }
        }

        private void updateCounter(String century, Context context) {

            N_COUNTER currentCentury = N_COUNTER.valueOf("N_" + century);
            Counter counter = null;
            switch (currentCentury) {
                case N_190:
                    counter = context.getCounter(N_COUNTER.N_190);
                    break;
                case N_191:
                    counter = context.getCounter(N_COUNTER.N_191);
                    break;
                case N_192:
                    counter = context.getCounter(N_COUNTER.N_192);
                    break;
                case N_193:
                    counter = context.getCounter(N_COUNTER.N_193);
                    break;
                case N_194:
                    counter = context.getCounter(N_COUNTER.N_194);
                    break;
                case N_195:
                    counter = context.getCounter(N_COUNTER.N_195);
                    break;
                case N_196:
                    counter = context.getCounter(N_COUNTER.N_196);
                    break;
                case N_197:
                    counter = context.getCounter(N_COUNTER.N_197);
                    break;
                case N_198:
                    counter = context.getCounter(N_COUNTER.N_198);
                    break;
                case N_199:
                    counter = context.getCounter(N_COUNTER.N_199);
                    break;
                case N_200:
                    counter = context.getCounter(N_COUNTER.N_200);
                    break;
                case N_201:
                    counter = context.getCounter(N_COUNTER.N_201);
                    break;
            }

            if (counter != null) {
                counter.increment(cw);
            }
        }
    }


    // Google N-Gram reader.
    public static class MySequenceFileInputFormat extends SequenceFileInputFormat<LongWritable,Text> {}


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();

        conf.set("mapred.reduce.slowstart.completed.maps", "1");
        // conf.set("pairsPerCentury", args[2]);  // TODO change from 2 to 3 for amazon.
        // conf.set("mapred.map.tasks", "10");
        // conf.set("mapred.reduce.tasks", "2");

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
