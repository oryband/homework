package com.dsp.ass3;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
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


public class Vector {

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Vector.class.getName()));

    // Mapper just passes on data.
    public static class MapClass extends Mapper<LongWritable, Text, Text, Text> {

        // intwritable ??
        private Map<String, Integer> headers = new HashMap <String, Integer>();

        private String HeadersLink = "";
        private String depTree;

        private Text newKey = new Text(),
                newValue = new Text();

        @Override
        public void setup(Context context) {
            initHeader(Utils.LinkToString(HeadersLink));
        }

        private void initHeader(String data) {
            String[] splitData = data.split("\n");
            for (int i = 0 ; i < splitData.length ; i++){
                depTree = splitData[i].substring(0, splitData[i].lastIndexOf(Utils.keyValueDelim));
                headers.put(depTree, Integer.valueOf(i));
            }
        }

        private String getOccrences(String dep) {
            return dep.split("\t")[1].split(" ")[2];
        }

        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            StringBuilder sb = new StringBuilder();

            // Fetch key/value and emit them.
            String v = value.toString();
            Integer depTreeIndex;
            int index = v.indexOf(Utils.keyValueDelim, v.indexOf(Utils.keyValueDelim) + 1);

            String[] depTrees = v.substring(index + 1).split("\t\t");

            sb.append("{ ");
            for (int i = 0 ; i < depTrees.length ; i++) {
                depTreeIndex = headers.get(depTrees[i]);
                if (depTreeIndex != null) {
                    //TODO should we print depTrees[i] ? or weka.blabla(depTrees[i])
                    sb.append(depTreeIndex.toString()).append(Utils.delim).append(getOccrences(depTrees[i]));
                    if (i != depTrees.length -1){
                        sb.append(",");
                    }
                }
            }
            sb.append("}");

            newValue.set(sb.toString());
            newKey.set(v.substring(0, index));

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

        @Override
        public void reduce(Text key, Iterable<Text> values, Context context)
            throws IOException, InterruptedException {

            // should not contain more then 1 value for each key.
            for (Text value : values) {
                context.write(key, value);
            }
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();

        Job job = new Job(conf, "Vector");

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
