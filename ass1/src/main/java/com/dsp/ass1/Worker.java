package com.dsp.ass1;

import java.util.List;
import java.util.logging.Logger;

import java.net.URL;

import java.io.IOException;
import java.io.FileNotFoundException;

import java.io.File;
import java.io.PrintWriter;
import java.awt.image.BufferedImage;

import javax.imageio.ImageIO;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.util.PDFTextStripper;
import org.apache.pdfbox.util.PDFText2HTML;

import org.apache.commons.io.FilenameUtils;

import java.util.List;
import java.util.UUID;
import java.util.Map.Entry;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.CreateQueueRequest;
import com.amazonaws.services.sqs.model.DeleteMessageRequest;
import com.amazonaws.services.sqs.model.DeleteQueueRequest;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;
import com.amazonaws.services.sqs.model.SendMessageRequest;


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


    public static void toHTML(PDDocument doc, String base) {
        String result;
        PrintWriter out;
        PDFTextStripper htmlstripper = null;

        try {
            htmlstripper = new PDFText2HTML("utf-8");
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return;
        }

        try {
            result = htmlstripper.getText(doc).trim();
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return;
        }

        try {
            out = new PrintWriter(base + ".html");
        } catch (FileNotFoundException e) {
            logger.severe(e.getMessage());
            return;
        }

        out.println(result);
        out.close();
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
            else if (action.equals("ToHTML")) {
                toHTML(doc,base);
            }
        }

        try {
            doc.close();
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }
    }


    public static void main(String[] args) throws Exception {

        String MissionsUrl =  "https://sqs.us-west-2.amazonaws.com/340657073537/Missions";
        AmazonSQS sqsMissions = new AmazonSQSClient(new PropertiesCredentials(Worker.class.getResourceAsStream("AwsCredentials.properties")));
        String FinishedUrl =  "https://sqs.us-west-2.amazonaws.com/340657073537/Finished";
        AmazonSQS sqsFinished = new AmazonSQSClient(new PropertiesCredentials(Worker.class.getResourceAsStream("AwsCredentials.properties")));

        try {
            ReceiveMessageRequest receiveMessageRequest = new ReceiveMessageRequest(MissionsUrl);
            logger.info("Receiving new message from sqsMissions.\n");
            List<Message> messages = sqsMissions.receiveMessage(receiveMessageRequest).getMessages();
          //System.out.println("Got "+messages.size());
            while (messages.size()>0){
                Message message=messages.get(0);
                logger.info("Got message from sqsMissions : "+message.getBody()+ " .\n");
                String[] messageSpliter= message.getBody().split("\t");
                String action = messageSpliter[0];
                String Url = messageSpliter[1];
                handlePage(action,Url);
                logger.info("Finished "+action+" "+Url);
                
                // Deleting message
                logger.info("Deleting : "+ message.getBody() + "from sqsMissions.\n");
                String messageRecieptHandle = message.getReceiptHandle();
                sqsMissions.deleteMessage(new DeleteMessageRequest(MissionsUrl, messageRecieptHandle));
                
                // Send a message
                logger.info("Sending : Finished "+message.getBody()+" to sqsFinished.\n");
                sqsFinished.sendMessage(new SendMessageRequest(FinishedUrl, "Finish: "+action+" "+Url));
                
                // getting new message
                logger.info("Receiving new message from sqsMissions.\n");
                messages = sqsMissions.receiveMessage(receiveMessageRequest).getMessages();
            }
            System.out.println("Done!.\n");
        } catch (AmazonServiceException ase) {
            logger.severe(ase.getMessage());
        } catch (AmazonClientException ace) {
            logger.severe(ace.getMessage());
        }
    }
}