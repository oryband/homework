package com.dsp.ass1;

import java.util.logging.Logger;
import java.util.logging.ConsoleHandler;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import com.amazonaws.AmazonClientException;
import com.amazonaws.auth.PropertiesCredentials;
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


    public static String uploadFileToS3(AmazonS3 s3, String fileName, String info)
            throws IOException {
        String fileAddress = getS3FileAddress(s3, fileName);
        File file = createSampleFile(info);

        if (file != null || fileAddress == null) {
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
        return null;
    }

    // TODO Write to stream and then save it to file,
    // INSTEAD of creating a file and writing to it. >:(
    private static File createSampleFile(String info) throws IOException {
        File file;

        try {
            file = File.createTempFile("aws-java-sdk-", ".txt");
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }

        file.deleteOnExit();
        Writer writer = new OutputStreamWriter(new FileOutputStream(file));
        writer.write(info);

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

        return "https://s3-" + address + ".amazonaws.com/" + bucket + "/"
        + path + fileName;
    }

    public static PropertiesCredentials loadCredentials() {
        try {
            return new PropertiesCredentials(
                    Utils.class
                    .getResourceAsStream("/AWSCredentials.properties"));
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

    public static void deleteTaskMessage(Message msg, String sqsUrl,
            AmazonSQS sqs) {
        String handle = msg.getReceiptHandle();

        try {
            sqs.deleteMessage(new DeleteMessageRequest(sqsUrl, handle));
            logger.info("deleted: " + msg.getBody());
        } catch (AmazonClientException e) {
            logger.severe(e.getMessage());
        }
    }

    public static List<Message> getMessages(ReceiveMessageRequest req,
            AmazonSQS sqs) {
        List<Message> msgs;

        try {
            msgs = sqs.receiveMessage(req).getMessages();
        } catch (AmazonClientException e) {
            logger.severe(e.getMessage());
            return null;
        }

        return msgs;
    }

    public static String LinkToString(String link) throws IOException {
        URL url;
        String line;
        StringBuilder content = new StringBuilder();
        InputStream is;
        BufferedReader br;

        try {
            url = new URL(link);
        } catch (MalformedURLException e) {
            logger.severe(e.getMessage());
            return null;
        }

        try {
            is = url.openStream();
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }

        br = new BufferedReader(new InputStreamReader(is));

        while ( (line = br.readLine()) != null) {
            content.append(line);
            content.append("\n");
        }

        try {
            br.close();
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }

        try {
            is.close();
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }

        return content.toString();
    }

}
