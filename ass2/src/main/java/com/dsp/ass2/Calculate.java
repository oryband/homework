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


public class Calculate {

    public static class MapClass extends Mapper<LongWritable, Text, MyData, Text> {

        private MyData newKey = new MyData();
        private Text newValue = new Text();

        // Write { <w1,w2> : century , w1, c(w1), c(w1,w2) }
        @Override
        public void map(LongWritable key, Text value, Context context)
                throws IOException, InterruptedException {

            // Fetch words from value.
            String[] words = value.toString().split(Utils.delim);
            String century = words[0],
                    w1 = words[1],
                    w2 = words[2],
                    cW1 = words[3],
                    cW2 = words[4],
                    cW1W2 = words[5],
                    N = context.getConfiguration().get("N_" + century, "-1"),
                    PMI;

            if (N.equals("-1"))
                return;

            PMI = calculatePMI(Double.parseDouble(cW1), Double.parseDouble(cW2), Double.parseDouble(cW1W2), Double.parseDouble(N));

            newValue.set(w1 + Utils.delim + w2);

            newKey.set(century, PMI);
            context.write(newKey, newValue);
        }

        private static String calculatePMI(double cW1, double cW2, double cW1W2, double N) {
            double answer = Math.log(cW1W2) + Math.log(N) - Math.log(cW1) - Math.log(cW2);
            return String.valueOf(answer);
        }
    }


    public static class PartitionerClass extends Partitioner<MyData, Text> {
        // TODO make this smarter.
        @Override
        public int getPartition(MyData key, Text value, int numPartitions) {
            int century = Integer.parseInt(key.century);
            return (century - 190) %  numPartitions; // its good ??
            // return century %  numPartitions;
        }

    }


    public static class ReduceClass extends Reducer<MyData,Text,Text,Text> {

        private Text newKey = new Text();
        private int[] pairsPerDecade = new int[12];
        @Override

        public void setup(Context context){
            int pairsNum = Integer.parseInt(context.getConfiguration().get("pairsPerDecade", "-1"));
            for (int i = 0 ; i < 12 ; i++) {
                pairsPerDecade[i] = pairsNum;
            }
        }

        public void reduce(MyData key, Iterable<Text> values, Context context)
                throws IOException, InterruptedException {

            String  century = key.century;

            int place = Integer.parseInt(century) - 190;
            if (pairsPerDecade[place] > 0) {
                newKey.set(key.century + Utils.delim + key.PMI);
                context.write(newKey, values.iterator().next());
                pairsPerDecade[place]--;
            }
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        conf.set("mapred.reduce.slowstart.completed.maps", "1");
        //conf.set("mapred.map.tasks","10");
        //conf.set("mapred.reduce.tasks","2");

        Job job = new Job(conf, "Join");

        job.setJarByClass(Calculate.class);
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
