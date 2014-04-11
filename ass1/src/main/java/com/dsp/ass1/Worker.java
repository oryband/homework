package com.dsp.ass1;

import java.util.List;
import java.util.logging.Logger;

import java.net.URL;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import java.io.File;

import java.awt.image.BufferedImage;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.util.PDFTextStripper;
import org.apache.pdfbox.util.PDFText2HTML;

import org.apache.commons.io.FilenameUtils;

import javax.imageio.ImageIO;

import java.util.List;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.CannedAccessControlList;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.DeleteMessageRequest;
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


    public static String toImage(PDPage page, String base, AmazonS3 s3,String bucketName,String path) {
        String fileName = base + ".png",
               answer = null;
        logger.info("image: " + fileName);

        try {
            BufferedImage img = page.convertToImage();
            answer = uploadFileToS3(s3,bucketName,path,fileName,img.toString());
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }
        return answer;
    }


    public static String toHTML(PDDocument doc, String base,AmazonS3 s3,String bucketName,String path) throws IOException {
        String fileName = base + ".html";
        logger.info("html: " + fileName);

        String pageText,
               answer=null;
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
            answer = uploadFileToS3(s3,bucketName,path,fileName, pageText);
        } catch (FileNotFoundException e) {
            logger.severe(e.getMessage());
            return null;
        }

        return answer;
    }


    public static String toText(PDDocument doc, String base,AmazonS3 s3,String bucketName,String path) throws IOException {
        String fileName = base + ".txt";
        logger.info("text: " + fileName);

        PDFTextStripper stripper;
        String pageText;
        String answer=null;

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
            answer = uploadFileToS3(s3,bucketName,path,fileName, pageText);
        } catch (FileNotFoundException e) {
            logger.severe(e.getMessage());
            return null;
        }
        return answer;

    }


    public static String handleDocument(String action, String url,AmazonS3 s3,String bucketName,String path) throws IOException {
        logger.info("handling: " + action + "\t" + url);

        String base = FilenameUtils.getBaseName(url),
               answer=null;
        PDDocument doc = getDocument(url);

        if (doc == null) {
            return null;
        } else {
            if (action.equals("ToImage")) {
                PDPage page = (PDPage) doc.getDocumentCatalog().getAllPages().get(0);
                answer =  toImage(page, base,s3,bucketName,path);
            }
            else if (action.equals("ToText")) {
                answer = toText(doc, base,s3,bucketName,path);
            }
            else if (action.equals("ToHTML")) {
                answer =  toHTML(doc,base,s3,bucketName,path);
            }
        }

        try {
            doc.close();
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }

        logger.info("finished handling: " + action + "\t" + url);
        return answer;
    }


    public static void deleteTaskMessage(Message msg, String sqsUrl, AmazonSQS sqs) {
        String handle = msg.getReceiptHandle();
        sqs.deleteMessage(new DeleteMessageRequest(sqsUrl, handle));
        logger.info("deleted: " + msg.getBody());
    }


    public static void sendFinishedMessage(Message msg,String pos, String sqsUrl, AmazonSQS sqs) {
        String[] splitter= msg.getBody().split("\t");
        String action = splitter[1],
               url = splitter[2],
               reply = "done PDF task " + action + " " + url+"\nnew Url: "+pos;  // TODO need to add s3 location of result, according to instructions.

        sqs.sendMessage(new SendMessageRequest(sqsUrl, reply));
        logger.info(reply);
    }
    
    
    public static String uploadFileToS3(AmazonS3 s3,String bucketName, String path,String fileName, String info) throws IOException {
        String s3Adress= s3.getBucketLocation(bucketName),
               fileAdress = "https://s3-"+s3Adress+".amazonaws.com/"+bucketName+"/"+path+fileName;
        PutObjectRequest up = new PutObjectRequest(bucketName, path+fileName, createSampleFile(info));
        s3.putObject(up.withCannedAcl(CannedAccessControlList.PublicRead));
        logger.info("filed saved: " +fileAdress);
        return fileAdress;
    }
    
    
    public static String handleMessage(Message msg,AmazonS3 s3,String bucketName,String path) throws IOException {
        String body = msg.getBody();
        logger.info("received: " + body);

        String[] parts = msg.getBody().split("\t");
        String type, action, url;

        if (parts.length == 3) {
            type = parts[0];
            action = parts[1];
            url = parts[2];
        } else {
            logger.info("ignoring: " + body);
            return null;
        }

        if (type.equals("new PDF task")) {
            return handleDocument(action, url,s3,bucketName,path); 
        } else {
            logger.info("ignoring: " + body);
            return null;
        }
    }


    public static List<Message> getMessages(ReceiveMessageRequest req, AmazonSQS sqs) {
        List<Message> msgs = sqs.receiveMessage(req).getMessages();
        return msgs;
    }
    
    
    private static File createSampleFile(String info) throws IOException {
        File file = File.createTempFile("aws-java-sdk-", ".txt");
        file.deleteOnExit();
        Writer writer = new OutputStreamWriter(new FileOutputStream(file));
        writer.write(info);
        writer.close();
        return file;
    }

    public static void main(String[] args) throws Exception {
        logger.info("Starting...");
        String missionsUrl = "https://sqs.us-west-2.amazonaws.com/340657073537/tasks",
               finishedUrl = "https://sqs.us-west-2.amazonaws.com/340657073537/finished",
               bucketName = "dsp-ass1",
               path = "here/";

        AWSCredentials creds = new PropertiesCredentials(
                Worker.class.getResourceAsStream("/AWSCredentials.properties"));
        AmazonSQS sqsMissions = new AmazonSQSClient(creds),
                  sqsFinished = new AmazonSQSClient(creds);

        AmazonS3 s3 = new AmazonS3Client(creds);
        ReceiveMessageRequest req;

        List<Message> msgs;

        try {
            req = new ReceiveMessageRequest(missionsUrl);
        } catch (AmazonServiceException ase) {
            logger.severe(ase.getMessage());
            return;
        } catch (AmazonClientException ace) {
            logger.severe(ace.getMessage());
            return;
        }
        
        msgs = getMessages(req, sqsMissions);
        while (msgs.size() > 0) {
            Message msg = msgs.get(0);
            String pos = handleMessage(msg,s3,bucketName,path);
            if (pos != null) {
                deleteTaskMessage(msg, missionsUrl, sqsMissions);
                sendFinishedMessage(msg,pos, finishedUrl, sqsFinished);
            }

            msgs = getMessages(req, sqsMissions);
        }

        logger.info("finished messages.");
    }
}
