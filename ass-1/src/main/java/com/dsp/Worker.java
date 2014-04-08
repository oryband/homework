package com.dsp.ass1;

import java.util.StringTokenizer;
import java.util.AbstractMap.SimpleEntry;
import java.util.logging.Logger;

import java.net.URL;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.FileNotFoundException;

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
    private static final Logger logger = Logger.getLogger(Worker.class.getName());

    public static PDDocument getDocument(String url) {
        PDDocument doc;

        try {
            doc = PDDocument.load(new URL(url));
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }

        return doc;
    }


    private static String getText(PDDocument doc) throws IOException {
        PDFTextStripper reader = new PDFTextStripper();
        reader.setStartPage(1);
        reader.setEndPage(1);
        return reader.getText(doc);
    }


    public static void toImage(PDPage page, String base) {
        try {
            BufferedImage image = page.convertToImage();
            File outputFile = new File(base + ".png");
            ImageIO.write(image, "png", outputFile);
        } catch (IOException e) {}  // TODO same
    }


    public static void toText(PDDocument doc, String base) {
        String pageText;
        PrintWriter out;

        try {
            pageText = getText(doc);
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return;
        }

        try {
            out = new PrintWriter(base + ".txt");
        } catch (FileNotFoundException e) {
            logger.severe(e.getMessage());
            return;
        }

        out.println(pageText);
        out.close();
    }


    public static void handlePage(String action, String url) {
        logger.info(action + " " + url);

        String base = FilenameUtils.getBaseName(url);  // url file base name.
        PDDocument doc = getDocument(url);

        if (doc == null) {
            return;
        } else {
            if (action.equals("ToImage")) {
                PDPage page = (PDPage) doc.getDocumentCatalog().getAllPages().get(0);
                toImage(page, base);
            }
            else if (action.equals("ToText")) {
                toText(doc, base);
            }

            try {
                doc.close();
            } catch (IOException e) {
                logger.severe(e.getMessage());
            }
        }
    }


    public static void main(String[] args) throws IOException, InterruptedException {
        String action = args[0],
               url = args[1];

        handlePage(action, url);
    }
}
