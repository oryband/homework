package com.dsp.ass3;

import java.io.BufferedReader;
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

    private static String headersUrl = "s3://ory-dsp-ass3/steps/headers/output/headers",
            startOfVector = "{",
            endOfVector = "}",
            arffCoordinateDelim = ",";


    private static final Logger logger = Utils.setLogger(Logger.getLogger(Vector.class.getName()));


    // Write { N1, N2, hypernym-index : { i1:h1, i2:h2, i3:h3,...} }
    // Where [..] is the pair's SPARSE dep-tree vector.
    public static class MapClass extends Mapper<LongWritable, Text, Text, Text> {

        // ARFF header label map.
        private Map<String, Integer> labels = new HashMap <String, Integer>();
        private String depTree;

        private Text newKey = new Text(),
                newValue = new Text();


        // Init headers labels map.
        @Override
        public void setup(Context context) {
            // Fetch label data from url.
            BufferedReader br = Utils.linkToBufferedReader(headersUrl);

            int i=0;
            String line;
            try {
                // Read dep-trees and set labels in map.
                while ( (line = br.readLine()) != null) {
                    // Read dep-tree.
                    depTree = line.substring(0, line.lastIndexOf(Utils.biarcDelim));

                    // Set coordinate label in labels map.
                    labels.put(depTree, Integer.valueOf(i));

                    // Set label coordinate index in vector.
                    i++;
                }
            } catch (IOException e) {
                logger.severe(e.getMessage());
                return;
            }

            // Close buffer.
            try {
                br.close();
            } catch (IOException e) {
                logger.severe(e.getMessage());
                return;
            }
        }


        private String getHits(String dep) {
            return dep.split(Utils.biarcDelim)[1].split(Utils.delim)[2];
        }


        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            StringBuilder sb = new StringBuilder();

            // Fetch key/value and emit.
            String v = value.toString();
            Integer depTreeIndex;

            // Fetch the index of the '\t' before the first biarc label.
            int index = v.indexOf(Utils.biarcDelim, v.indexOf(Utils.biarcDelim) + 1);

            // Write SPARSE vector with coordinate indexes.
            // We use the labels map (init in setup() ) to fetch each cooridnate label's index.
            String[] depTrees = v.substring(index + 1).split(Utils.coordinateDelim);
            for (int i=0; i < depTrees.length; i++) {
                // Fetch dep-tree index from labels map.
                depTreeIndex = labels.get(depTrees[i]);

                // Write only non-zero coordinates (this is a SPARSE vector).
                if (depTreeIndex != null) {
                    // Prepend beginning of vector char.
                    sb.append(startOfVector);

                    // Append coordinate index and value.
                    sb.append(depTreeIndex.toString()).append(Utils.delim).append(getHits(depTrees[i]));

                    // Append coordinate delimeter if this isn't the last
                    // cooridnate.
                    if (i != depTrees.length -1) {
                        sb.append(arffCoordinateDelim);
                    }

                    // Append end of vector char.
                    sb.append(endOfVector).append(Utils.lineDelim);
                }
            }

            // Fetch noun-pair key and set as key.
            newKey.set(v.substring(0, index));

            // Set vector as value.
            newValue.set(sb.toString());

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
