package com.dsp.ass1;

import java.util.List;
import java.util.logging.Logger;
import java.util.concurrent.TimeUnit;

import java.net.URL;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.InputStream;

import java.awt.image.BufferedImage;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.util.PDFTextStripper;
import org.apache.pdfbox.util.PDFText2HTML;

import org.apache.commons.io.FilenameUtils;

import javax.imageio.ImageIO;

import com.amazonaws.AmazonClientException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.CannedAccessControlList;
import com.amazonaws.services.s3.model.ObjectMetadata;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.DeleteMessageRequest;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;


public class Worker {
    private static final Logger logger = Logger.getLogger(Worker.class.getName());


    // Returns a PDDocument from a string URL.
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


    public static String toImage(PDPage page, String base, AmazonS3 s3, String bucket, String path) {
        String fileName = base + ".png";
        BufferedImage img;

        logger.info("image: " + fileName);

        try {
            img = page.convertToImage();  // PDF to PNG image.
            return uploadImageToS3(s3, bucket, path, fileName, img);
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }
    }


    public static String toHTML(PDDocument doc, String base, AmazonS3 s3, String bucket, String path)
        throws IOException {

        String fileName = base + ".html";
        logger.info("html: " + fileName);

        String pageText;
        PDFTextStripper stripper;

        try {
            stripper = new PDFText2HTML("utf-8");
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }

        try {
            pageText = stripper.getText(doc);
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }

        try {
            return Utils.uploadFileToS3(s3, bucket, path, fileName, pageText);
        } catch (FileNotFoundException e) {
            logger.severe(e.getMessage());
            return null;
        }
    }


    public static String toText(PDDocument doc, String base, AmazonS3 s3, String bucket, String path)
        throws IOException {

        String fileName = base + ".txt";
        logger.info("text: " + fileName);

        PDFTextStripper stripper;
        String pageText;

        try {
            stripper = new PDFTextStripper();
            stripper.setStartPage(1);
            stripper.setEndPage(1);
            pageText = stripper.getText(doc);
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }

        try {
            return Utils.uploadFileToS3(s3, bucket, path, fileName, pageText);
        } catch (FileNotFoundException e) {
            logger.severe(e.getMessage());
            return null;
        }
    }


    public static String handleDocument(String action, String url, AmazonS3 s3, String bucket, String path)
        throws IOException {

        logger.info("handling: " + action + "\t" + url);

        String base = FilenameUtils.getBaseName(url),
               result = null;
        PDDocument doc = getDocument(url);

        if (doc == null) {
            return null;
        } else {
            if (action.equals("ToImage")) {
                PDPage page = (PDPage) doc.getDocumentCatalog().getAllPages().get(0);
                result = toImage(page, base, s3, bucket, path);
            }
            else if (action.equals("ToText")) {
                result = toText(doc, base, s3, bucket, path);
            }
            else if (action.equals("ToHTML")) {
                result = toHTML(doc, base, s3, bucket, path);
            }
        }

        logger.info("finished handling: " + action + "\t" + url);

        try {
            doc.close();
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }

        return result;
    }


    public static void deleteTaskMessage(Message msg, String sqsUrl, AmazonSQS sqs) {
        String handle = msg.getReceiptHandle();

        try {
            sqs.deleteMessage(new DeleteMessageRequest(sqsUrl, handle));
            logger.info("deleted: " + msg.getBody());
        } catch (AmazonClientException e){
            logger.severe(e.getMessage());
        }
    }


    public static void sendFinishedMessage(Message msg, String pos, String sqsUrl, AmazonSQS sqs) {

        if (pos.equals("shutdown")){
            Utils.sendMessage(sqs,sqsUrl,"shutting down...");
        }

        else {
            String[] splitter= msg.getBody().split("\t");
            String action = splitter[1],
                   url = splitter[2],
                   // TODO need to add s3 location of result, according to instructions.
                   reply = "done PDF task\t" + action + "\t" + url + "\t" + pos;

            Utils.sendMessage(sqs,sqsUrl,reply);
        }
    }


    public static String uploadImageToS3(AmazonS3 s3, String bucket, String path, String fileName, BufferedImage img)
        throws IOException {

        // Get S3 file address (file doesn't exist yet, we're going to save the data to this address.)
        String address = Utils.getS3FileAddress(s3, bucket, path, fileName);

        if (address != null) {
            ByteArrayOutputStream outStream = new ByteArrayOutputStream();

            // Write image to file.
            try {
                ImageIO.write(img, "png", outStream);
            } catch (IOException e) {
                logger.severe(e.getMessage());
                return null;
            }

            // Upload saved image to S3.
            byte[] buffer = outStream.toByteArray();
            ObjectMetadata meta = new ObjectMetadata();
            meta.setContentLength(buffer.length);
            InputStream inStream = new ByteArrayInputStream(buffer);
            PutObjectRequest request = new PutObjectRequest(bucket, path + fileName, inStream, meta);

            try {
                s3.putObject(request.withCannedAcl(CannedAccessControlList.PublicRead));
            } catch (AmazonClientException e) {
                logger.severe(e.getMessage());
                return null;
            }
        }

        return address;
    }


    public static String handleMessage(Message msg, AmazonS3 s3, String bucket, String path)
        throws IOException {

        String body = msg.getBody();
        logger.info("received: " + body);

        String[] parts = msg.getBody().split("\t");

        if (parts[0].equals("new PDF task") && parts.length >= 3) {
            return handleDocument(parts[1], parts[2], s3, bucket, path);
        // TODO Shutdown message should include a worker name (or tag name?)
        // so each worker will know the message if the message is intended
        // to him or noted to him or not. Something like this:
        // } else if (parts[0].equals("shutdown") && parts.length >= 2 && parts[1].equals(ami-identifier) {
        } else if (parts[0].equals("shutdown")) {
            return "shutdown";
        } else {
            logger.info("ignoring: " + body);
            return null;
        }
    }


    public static void main(String[] args) throws InterruptedException, IOException {
        Utils.setLogger(logger);

        logger.info("starting.");

        String missionsUrl = "https://sqs.us-west-2.amazonaws.com/340657073537/tasks",
               finishedUrl = "https://sqs.us-west-2.amazonaws.com/340657073537/finished",
               bucket = "dsp-ass1",
               path = "here/",
               result = "";

        AWSCredentials creds = Utils.loadCredentials();
        if(creds == null) {
            logger.severe("Can't find  the credentials file : closing...");
            return;
        }

        AmazonSQS sqsMissions = new AmazonSQSClient(creds),
                  sqsFinished = new AmazonSQSClient(creds);

        AmazonS3 s3 = new AmazonS3Client(creds);

        ReceiveMessageRequest req = new ReceiveMessageRequest(missionsUrl);

        List<Message> msgs;
        Message msg;

        // Process messages.
        msgs = Utils.getMessages(req, sqsMissions);
        while (true) {
            // Sleep if no messages arrived, and retry re-fetch new ones afterwards.
            if (msgs == null || msgs.size() == 0) {
                logger.info("no messages, sleeping.");

                try {
                    TimeUnit.SECONDS.sleep(5);
                } catch(InterruptedException e) {
                    logger.severe(e.getMessage());
                    result = "shutdown";
                }

            } else {
                msg = msgs.get(0);
                result = handleMessage(msg, s3, bucket, path);
                if (result != null) {
                    // Only handled messages are deleted.
                    deleteTaskMessage(msg, missionsUrl, sqsMissions);
                    sendFinishedMessage(msg, result, finishedUrl, sqsFinished);
                }
            }

            if (result != null && result.equals("shutdown")) {
                return;
            }

            msgs = Utils.getMessages(req, sqsMissions);
        }
    }
}
