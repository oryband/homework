import java.util.StringTokenizer;
import java.util.AbstractMap.SimpleEntry;
import java.net.URL;
import java.io.IOException;
import java.io.InterruptedException;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.util.PDFImageWriter;
import org.apache.pdfbox.util.PDFTextStripper;
import org.apache.pdfbox.util.PDFText2HTML;

import org.apache.commons.io.FilenameUtils;

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


public class PDF { 
    public static class MapClass extends Mapper<Text, Text, Text, Text> {
        private PDDocument document;
        private PDFTextStripper textStripper;
        private PDFImageWriter imageWriter;
        private PDFText2HTML text2HTML;

        @Override
        public void setup(Context context) throws IOException, InterruptedException {
            this.textStripper = new PDFTextStripper();
            this.imageWriter = new PDFImageWriter();
            this.text2HTML = new PDFText2HTML("utf-8");
        }

        @Override
        public void map(Text url, Text action, Context context) throws IOException, InterruptedException {
            try {
                this.document = PDDocument.load(new URL(url.toString())); 
            } catch (Exception e) {}  // TODO Handle specific exceptions.

            if (action.toString() == "toImage") {
                String base = FilenameUtils.getBaseName(url.toString()),
                       ext = FilenameUtils.getExtension(url.toString());
                try {
                    this.imageWriter.writeImage(this.document, "png", null, 1, 1, base + ext);
                } catch (IOException e) {}  // TODO same

                context.write();

            }
        }
    }
}
