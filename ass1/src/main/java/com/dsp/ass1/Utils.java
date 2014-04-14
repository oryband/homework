package com.dsp.ass1;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.List;
import java.util.logging.Logger;

import com.amazonaws.AmazonClientException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.CannedAccessControlList;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;
import com.amazonaws.services.sqs.model.SendMessageRequest;

public class Utils {
    
    private static final Logger logger = Logger.getLogger(Utils.class.getName());
    
    public static String uploadFileToS3(AmazonS3 s3, String bucket, String path, String fileName, String info)
            throws IOException {
            String fileAddress = getS3FileAddress(s3, bucket, path, fileName);
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
    
    // TODO Liran wtf is this function? Please document.
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
    
    public static String getS3FileAddress(AmazonS3 s3, String bucket, String path, String fileName) {
        String address;

        try {
            address = s3.getBucketLocation(bucket);
        } catch (AmazonClientException e) {
            logger.severe(e.getMessage());
            return null;
        }

        return "https://s3-" + address + ".amazonaws.com/" + bucket + "/" + path + fileName;
    }
    
    public static PropertiesCredentials loadCredentials(){
        try {
            return new PropertiesCredentials(Utils.class.getResourceAsStream("/AWSCredentials.properties"));
        } catch (IOException e){
            logger.severe(e.getMessage());
            return null;
        }
    }
    
    public static void SendMessageToQueue(AmazonSQS sqs, String sqsUrl, String info){
        
        try {
            sqs.sendMessage(new SendMessageRequest(sqsUrl, info));
            logger.info(info);
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
    
}
