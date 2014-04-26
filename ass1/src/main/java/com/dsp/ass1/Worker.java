package com.dsp.ass1;

import java.awt.image.BufferedImage;

import java.io.IOException;

import java.net.URL;

import java.util.List;
import java.util.logging.Logger;
import java.util.concurrent.TimeUnit;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.util.PDFTextStripper;
import org.apache.pdfbox.util.PDFText2HTML;

import org.apache.commons.io.FilenameUtils;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;

import com.amazonaws.auth.AWSCredentials;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.CannedAccessControlList;
import com.amazonaws.services.s3.model.PutObjectRequest;

import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.DeleteMessageRequest;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;


public class Worker {
    private static final Logger logger = Utils.setLogger(Logger.getLogger(Worker.class.getName()));


    // Returns a PDDocument from a string URL or null on error.
    private static PDDocument getDocument(String url) {
        PDDocument doc;

        try {
            doc = PDDocument.load(new URL(url));
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }

        return doc;
    }


    // Converts PDF file to PNG image and uploads it to S3.
    private static String toImage(AmazonS3 s3, PDPage page, String mission, String base) {
        String fileName = System.currentTimeMillis() + "_" + base + ".png";

        logger.info("Image: " + fileName);

        BufferedImage img;

        try {
            img = page.convertToImage();  // PDF to PNG image.
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return e.getMessage();
        }

        return Utils.uploadImageToS3(s3, Utils.filesPath, mission, fileName, img);
    }


    // Converts PDF to HTML and uploads it to S3.
    private static String toHTML(AmazonS3 s3, PDDocument doc, String mission, String base) {
        String fileName = System.currentTimeMillis() + "_" + base + ".html";

        logger.info("HTML: " + fileName);

        PDFTextStripper stripper;

        try {
            stripper = new PDFText2HTML("utf-8");  // encoding: utf-8
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return e.getMessage();
        }

        String data;
        try {
            data = stripper.getText(doc);  // TODO maybe horrible crashes occur here as well? not sure.
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return e.getMessage();
        }

        return Utils.uploadStringToS3(s3, Utils.filesPath, mission, fileName, data);
    }

    // Converts PDF to clear text file and uploads it to S3.
    private static String toText(AmazonS3 s3, PDDocument doc, String mission, String base) {
        String fileName = System.currentTimeMillis() + "_" + base + ".txt",
               data;

        logger.info("Text: " + fileName);

        PDFTextStripper stripper;

        try {
            stripper = new PDFTextStripper();
            stripper.setStartPage(1);
            stripper.setEndPage(1);

            data = stripper.getText(doc);  // TODO horrible crashes here.
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return e.getMessage();
        }

        return Utils.uploadStringToS3(s3, Utils.filesPath, mission, fileName, data);
    }


    // Receives an action, PDF file url, and S3 connection and process action
    // on PDF url. Afterwards uploads result to S3.
    private static String handleDocument(AmazonS3 s3, String action, String url, String mission) {
        logger.info("Handling document: " + action + "\t" + url + "\t" + mission);

        String base = FilenameUtils.getBaseName(url),
               result = "Unknown action";

        PDDocument doc = getDocument(url);

        if (doc == null) {
            return "Cant open PDF document";
        } else {
            if (action.equals("ToImage")) {
                if (doc.getDocumentCatalog().getAllPages().size() == 0) {
                    return "Failed to get page[0]";
                }

                PDPage page = (PDPage) doc.getDocumentCatalog().getAllPages().get(0);
                result = toImage(s3, page, mission, base);

            } else if (action.equals("ToText")) {
                result = toText(s3, doc, mission, base);

            } else if (action.equals("ToHTML")) {
                result = toHTML(s3, doc, mission, base);
            }
        }

        logger.info("Finished handling document: " + action + "\t" + url + "\t" + mission);

        try {
            doc.close();
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }

        return result;
    }


    // Sends a 'done PDF task ..' message to the queue.
    private static void sendFinishedMessage(Message msg, String pos, String sqsUrl, AmazonSQS sqs) {
        // action = split[1] , input = split[2], output = split[3], mission = split[4];  // TODO why split[4] is not used?
        String[] splitter = msg.getBody().split("\t");
        String reply = "done PDF task\t" + splitter[1] + "\t" + splitter[2] + "\t" + pos + "\t" + splitter[3];
        Utils.sendMessage(sqs, sqsUrl, reply);
    }


    // Sends a 'Failed PDF task ..' message to the queue.
    private static void sendFailedMessage(Message msg, String result, String sqsUrl, AmazonSQS sqs) {
        // action = split[1] , input = split[2], output = split[3], mission = split[4];
        String[] splitter = msg.getBody().split("\t");
        String reply = "failed PDF task\t" + splitter[1] + "\t" + splitter[2] + "\t" + result + "\t" + splitter[3];
        Utils.sendMessage(sqs, sqsUrl, reply);
    }


    // Processes (or ignores) messages.
    private static String handleTaskMessage(Message msg, AmazonS3 s3) {
        String body = msg.getBody(),
               result;

        if (body == null) {
            logger.severe("Error in message received, body is null.");
            return "Message body is null.";
        }

        logger.info("Message received: " + body);

        String[] parts = msg.getBody().split("\t");
        // parts[1] = action , parts [2] = link , parts[3] = mission counter

        if (parts[0].equals("new PDF task") && parts.length >= 4) {
            result = handleDocument(s3, parts[1], parts[2], parts[3]);
            if (result == null) {
                result = "Unknown error occured";
            }

            return result;

        } else  {
            logger.info("Ignoring message: " + body);
            return "Bad input";
        }
    }


    private static boolean handleShutdownMessage(Message msg, AmazonS3 s3) {
        String body = msg.getBody();
        if (body == null) {
            logger.severe("Error in message received, body is null.");
            return false;
        }

        logger.info("Shutdown message received: " + body);

        if (body.equals("shutdown")) {
            logger.info("Shutting down.");
            return true;
        } else  {
            logger.info("Ignoring: " + body);
            return false;
        }
    }


    private static void execute(AmazonSQS sqs, AmazonS3 s3){
        ReceiveMessageRequest taskReq = new ReceiveMessageRequest(Utils.tasksUrl),
                shutdownReq = new ReceiveMessageRequest(Utils.shutdownUrl);

        List<Message> taskMsgs = Utils.getMessages(taskReq, sqs),
            shutdownMsgs = Utils.getMessages(shutdownReq, sqs);

        Message msg;
        String result;

        // Process messages.
        while (true) {
            // Sleep if no messages arrived, and retry re-fetch new ones afterwards.
            if (Utils.isEmpty(taskMsgs) && Utils.isEmpty(shutdownMsgs)) {
                logger.info("No messages, sleeping.");

                try {
                    TimeUnit.SECONDS.sleep(5);
                } catch(InterruptedException e) {
                    logger.severe(e.getMessage());
                    result = "shutdown";
                }
            } else {
                if ( ! Utils.isEmpty(taskMsgs)){
                    msg = taskMsgs.get(0);
                    result = handleTaskMessage(msg, s3);
                    if (Utils.checkResult(result)) {
                        sendFinishedMessage(msg, result, Utils.finishedUrl, sqs);
                    } else {
                        sendFailedMessage(msg, result, Utils.finishedUrl, sqs);
                    }

                    Utils.deleteTaskMessage(msg, Utils.tasksUrl, sqs);
                }

                if ( ! Utils.isEmpty(shutdownMsgs)) {
                    msg = shutdownMsgs.get(0);
                    if (handleShutdownMessage(msg, s3)) {
                        Utils.deleteTaskMessage(msg, Utils.shutdownUrl, sqs);
                        break;
                    }
                }
            }

            taskMsgs = Utils.getMessages(taskReq, sqs);
            shutdownMsgs = Utils.getMessages(shutdownReq, sqs);
        }

        // Send a 'closing' message to the manager, so he could terminate the worker instance.
        Utils.sendMessage(sqs, Utils.closedWorkersUrl, "closed\t" + Utils.getHTML(Utils.instanceIdUrl));
    }


    public static void main(String[] args) {
        logger.info("Starting.");

        AWSCredentials creds = Utils.loadCredentials();
        if(creds == null) {
            logger.severe("Couldn't load credentials.");
            return;
        }

        AmazonSQS sqs = new AmazonSQSClient(creds);
        AmazonS3 s3 = Utils.createS3(creds);

        execute(sqs, s3);
    }
}
