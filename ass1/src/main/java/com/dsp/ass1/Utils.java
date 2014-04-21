package com.dsp.ass1;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import java.util.logging.Logger;
import java.util.logging.ConsoleHandler;
import java.util.List;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;

import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.auth.AWSCredentials;

import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.AmazonEC2Client;

import com.amazonaws.services.ec2.model.RunInstancesRequest;
import com.amazonaws.services.ec2.model.RunInstancesResult;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.CannedAccessControlList;
import com.amazonaws.services.s3.model.PutObjectRequest;

import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.model.DeleteMessageRequest;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;
import com.amazonaws.services.sqs.model.SendMessageRequest;


public class Utils {

    public static final String tasksUrl = "https://sqs.us-east-1.amazonaws.com/340657073537/tasks",
            finishedUrl = "https://sqs.us-east-1.amazonaws.com/340657073537/finished",
            localUrl = "https://sqs.us-east-1.amazonaws.com/340657073537/local",
            bucket = "dsp-ass1",
            path = "here/";

    private static final Logger logger = Logger.getLogger(Utils.class.getName());


    // Use custom string format for logger.
    public static void setLogger(Logger logger) {
        ShortFormatter formatter = new ShortFormatter();
        ConsoleHandler handler = new ConsoleHandler();

        logger.setUseParentHandlers(false);
        handler.setFormatter(formatter);
        logger.addHandler(handler);
    }


    public static String uploadFileToS3(AmazonS3 s3, String fileName, String info) {
        String fileAddress = getS3FileAddress(s3, fileName);
        File file = createSampleFile(info);

        if (file == null || fileAddress == null) {
            return null;
        } else {
            PutObjectRequest request = new PutObjectRequest(bucket, path + fileName, file);

            try {
                s3.putObject(request.withCannedAcl(CannedAccessControlList.PublicRead));
                logger.info("file saved: " + fileAddress);
            } catch (AmazonClientException e) {
                logger.severe(e.getMessage());
                return null;
            }
            return fileAddress;
        }
    }

    // TODO Write to stream and then save it to file,
    // INSTEAD of creating a file and writing to it. >:(
    private static File createSampleFile(String info) {
        File file;

        try {
            file = File.createTempFile("aws-java-sdk-", ".txt");
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }

        file.deleteOnExit();

        Writer writer;
        try {
            writer = new OutputStreamWriter(new FileOutputStream(file));
            writer.write(info);
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }

        try {
            writer.close();
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }

        return file;
    }


    // Fetches S3-file url.
    public static String getS3FileAddress(AmazonS3 s3, String fileName) {
        String address;

        try {
            address = s3.getBucketLocation(bucket);
        } catch (AmazonClientException e) {
            logger.severe(e.getMessage());
            return null;
        }

        return "https://s3-" + address + ".amazonaws.com/" + bucket + "/" + path + fileName;
    }


    public static PropertiesCredentials loadCredentials() {
        try {
            return new PropertiesCredentials(
                    Utils.class.getResourceAsStream("/AWSCredentials.properties"));
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }
    }


    public static void sendMessage(AmazonSQS sqs, String sqsUrl, String info) {
        try {
            sqs.sendMessage(new SendMessageRequest(sqsUrl, info));
            logger.info("message sent to queqe : " + info);
        } catch (AmazonClientException e) {
            logger.severe(e.getMessage());
        }
    }


    public static void deleteTaskMessage(Message msg, String sqsUrl, AmazonSQS sqs) {
        String handle = msg.getReceiptHandle();

        try {
            sqs.deleteMessage(new DeleteMessageRequest(sqsUrl, handle));
            logger.info("deleted: " + msg.getBody());
        } catch (AmazonClientException e) {
            logger.severe(e.getMessage());
        }
    }


    public static List<Message> getMessages(ReceiveMessageRequest req, AmazonSQS sqs) {
        List<Message> msgs;

        try {
            msgs = sqs.receiveMessage(req).getMessages();
        } catch (AmazonClientException e) {
            logger.severe(e.getMessage());
            return null;
        }

        return msgs;
    }


    public static RunInstancesResult createAmiFromSnapshot(int amount, String name, AWSCredentials creds) {

        AmazonEC2 ec2;

        ec2 = new AmazonEC2Client(creds);

        try {
            RunInstancesRequest request = new RunInstancesRequest();
            request.withImageId("ami-13a6bf7a")
                .withInstanceType("t1.micro")
                .withMinCount(amount)
                .withMaxCount(amount);

            return ec2.runInstances(request);

        } catch (AmazonServiceException e) {
            logger.severe(e.getMessage());
            return null;
        }
    }
}
