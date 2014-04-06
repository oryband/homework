package com.dsp.ass1;

import java.util.StringTokenizer;
import java.util.AbstractMap.SimpleEntry;

import java.net.URL;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.File;
import java.io.PrintWriter;

import java.awt.image.BufferedImage;

import javax.imageio.ImageIO;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.util.PDFImageWriter;
import org.apache.pdfbox.util.PDFTextStripper;
import org.apache.pdfbox.util.PDFText2HTML;

import org.apache.commons.io.FilenameUtils;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.Partitioner;

import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;


public class PDF {
    public static class MapClass extends Mapper<LongWritable, Text, Text, Text> {

        private String getText(PDDocument doc) throws IOException {
            PDFTextStripper reader = new PDFTextStripper();
            reader.setStartPage(1);
            reader.setEndPage(1);
            return reader.getText(doc);
        }

        @Override
        public void map(LongWritable index, Text line, Context context) throws IOException, InterruptedException {
            String fields[] = line.toString().split("\t"),
                   action = fields[0],
                   url = fields[1];
            PDDocument doc = PDDocument.load(new URL(url));
            PDPage page = (PDPage) doc.getDocumentCatalog().getAllPages().get(0);

            String base = FilenameUtils.getBaseName(url),
                   ext = FilenameUtils.getExtension(url),
                   outputFileName = base + ext;

            if (action == "toImage") {
                try {
                    BufferedImage image = page.convertToImage();
                    File outputfile = new File(outputFileName);
                    ImageIO.write(image, "png", outputfile);
                } catch (IOException e) {}  // TODO same
                context.write(new Text(action), new Text(outputFileName));

            } else if (action == "toText") {
                String pageText = this.getText(doc);
                PrintWriter out = new PrintWriter(outputFileName + ".txt");
                out.println(pageText);
                out.close();
                context.write(new Text(action), new Text(outputFileName));
            }

            doc.close();
        }
    }

    public static class PartitionerClass extends Partitioner<Text, Text> {
        @Override
        public int getPartition(Text action, Text outputFile, int numPartitions) {
            return 0 % numPartitions;  // TODO fix
        }
    }

    public static class ReduceClass extends Reducer<Text, Text, Text, Text> {
        @Override
        public void reduce(Text action, Iterable<Text> outputFiles, Context context) throws IOException,  InterruptedException {
            for (Text f : outputFiles) {
                context.write(action, f);
            }
        }
    }

    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();

        //conf.set("mapred.map.tasks","10");
        //conf.set("mapred.reduce.tasks","2");

        Job job = new Job(conf, "PDF");

        job.setJarByClass(PDF.class);
        job.setMapperClass(MapClass.class);
        job.setPartitionerClass(PartitionerClass.class);
        job.setCombinerClass(ReduceClass.class);
        job.setReducerClass(ReduceClass.class);

        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        // KeyValueTextInputFormat.addInputPath(job, new Path(args[0]));
        FileInputFormat.addInputPath(job, new Path(args[0]));
        FileOutputFormat.setOutputPath(job, new Path(args[1]));

        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
