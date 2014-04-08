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


public class Worker {

    private static String getText(PDDocument doc) throws IOException {
        PDFTextStripper reader = new PDFTextStripper();
        reader.setStartPage(1);
        reader.setEndPage(1);
        return reader.getText(doc);
    }

    public static void main(String[] args) throws IOException, InterruptedException {
        String action = args[0],
               url = args[1];

        PDDocument doc = PDDocument.load(new URL(url));
        PDPage page = (PDPage) doc.getDocumentCatalog().getAllPages().get(0);

        String outputFileName = FilenameUtils.getBaseName(url) + FilenameUtils.getExtension(url);

        if (action == "toImage") {
            try {
                BufferedImage image = page.convertToImage();
                File outputfile = new File(outputFileName);
                ImageIO.write(image, "png", outputfile);
            } catch (IOException e) {}  // TODO same

            System.out.println("image saved: " + action + " " + outputFileName);

        } else if (action == "toText") {
            String pageText = getText(doc);
            PrintWriter out = new PrintWriter(outputFileName + ".txt");
            out.println(pageText);
            out.close();

            System.out.println("text saved: " + action + " " + outputFileName);
        }

        doc.close();
    }
}
