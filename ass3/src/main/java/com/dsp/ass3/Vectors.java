package com.dsp.ass3;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
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

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.s3.AmazonS3;


public class Vectors {

    private static String labelsPath = "steps/labels/output/test-labels",
    // private static String labelsPath = "steps/labels/output/labels",
            startOfVectors = "{",
            endOfVectors = "}",
            arffCoordinateDelim = ",",
            classCoordinateIndex = "0";


    private static final Logger logger = Utils.setLogger(Logger.getLogger(Vectors.class.getName()));


    // Write { N1, N2, hypernym-index : { class, i1:h1, i2:h2, i3:h3,...} }
    // Where [..] is the pair's SPARSE dep-tree vector.
    public static class MapClass extends Mapper<LongWritable, Text, Text, Text> {

        // ARFF header label map.
        private Map<String, Integer> labels = new HashMap <String, Integer>();

        // For downloading labels from S3.
        private AWSCredentials creds;
        private AmazonS3 s3;

        private Text newKey = new Text(),
                newValue = new Text();


        // Init headers labels map.
        @Override
        public void setup(Context context) {
            creds = Utils.loadCredentials();
            if (creds == null) {
                logger.severe("Couldn't load credentials.");
                return;
            }

            s3 = Utils.createS3(creds);

            // Fetch label data from url.
            InputStream is = Utils.readFromS3(s3, labelsPath);
            if (is == null) {
                return;
            }

            // Read input.
            BufferedReader br = new BufferedReader(new InputStreamReader(is));

            int i=1;
            String depTree, line;
            try {
                // Read dep-trees and set labels in map.
                while ( (line = br.readLine()) != null) {
                    // Read dep-tree, and filter postfixed '\t' char at the end.
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

            // Close input stream.
            try {
                is.close();
            } catch (IOException e) {
                logger.severe(e.getMessage());
            }

            // Close buffer.
            try {
                br.close();
            } catch (IOException e) {
                logger.severe(e.getMessage());
                return;
            }
        }


        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            StringBuilder sb = new StringBuilder();

            // Fetch key/value and emit.
            String v = value.toString();

            // Fetch the index of the '\t' before the first biarc label.
            int classIndex = v.indexOf(Utils.biarcDelim),
                dataIndex = v.indexOf(Utils.biarcDelim, classIndex + 1);

            boolean related = v.substring(classIndex + 1, dataIndex).equals("true");

            // Prepend beginning of vector char.
            sb.append(startOfVectors);

            // Append un/related as index 0.
            sb.append(classCoordinateIndex).append(Utils.delim).append(related);

            // Skip noun-pairs with no hits.
            String[] depTrees = v.substring(dataIndex + 1).split(Utils.coordinateDelim);
            if (depTrees.length == 0) {
                return;
            }

            String depTree, hits;
            int hitsIndex;
            Integer depTreeIndex;

            // Write SPARSE vector with coordinate indexes.
            // We use the labels map (init in setup() ) to fetch each cooridnate label's index.
            for (int i=0; i < depTrees.length; i++) {
                // Fetch dep-tree, coordinate index, and hits.
                depTree = depTrees[i];

                hitsIndex = depTree.lastIndexOf(Utils.keyValueDelim);
                hits = depTree.substring(hitsIndex +1);

                depTree = depTree.substring(0, hitsIndex);
                depTreeIndex = labels.get(depTree);

                // Write only non-zero coordinates (this is a SPARSE vector).
                if (depTreeIndex != null) {
                    // Append coordinate index and value.
                    sb.append(arffCoordinateDelim).append(depTreeIndex.toString()).append(Utils.delim).append(hits);


                }
            }
            // Append end of vector char.
            sb.append(endOfVectors);

            // Fetch noun-pair key and set as key.
            newKey.set(v.substring(0, classIndex));

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

            // Emit for each value element (in case for some reason we have more).
            for (Text value : values) {
                context.write(key, value);
            }
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();

        Job job = new Job(conf, "Vectors");

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
