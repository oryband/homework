package com.dsp.ass1;

import java.util.List;
import java.util.UUID;
import java.util.Map.Entry;
import java.util.logging.Logger;
import java.util.logging.ConsoleHandler;
import java.util.concurrent.TimeUnit;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.PropertiesCredentials;

import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.CreateQueueRequest;
import com.amazonaws.services.sqs.model.DeleteMessageRequest;
import com.amazonaws.services.sqs.model.DeleteQueueRequest;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;
import com.amazonaws.services.sqs.model.SendMessageRequest;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.GetObjectRequest;
import com.amazonaws.services.s3.model.ListObjectsRequest;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.s3.model.Bucket;
import com.amazonaws.services.s3.model.S3Object;
import com.amazonaws.services.s3.model.ObjectListing;
import com.amazonaws.services.s3.model.S3ObjectSummary;


public class Manager {
    private static final Logger logger = Logger.getLogger(Worker.class.getName());

    public static String handleMessage(Message msg, AmazonS3 s3, String bucket, String path)
        throws IOException {

        return null;
    }
    public static void deleteTaskMessage(Message msg, String sqsUrl, AmazonSQS sqs) {
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

    public static void main(String[] args) throws InterruptedException, IOException {
        // Use custom string format for logger.
        ShortFormatter formatter = new ShortFormatter();
        ConsoleHandler handler = new ConsoleHandler();

        logger.setUseParentHandlers(false);
        handler.setFormatter(formatter);
        logger.addHandler(handler);

        logger.info("starting.");

        String tasksUrl = "https://sqs.us-west-2.amazonaws.com/340657073537/tasks",
               finishedUrl = "https://sqs.us-west-2.amazonaws.com/340657073537/finished",
               localUrl = "https://sqs.us-west-2.amazonaws.com/340657073537/local",
               bucket = "dsp-ass1",
               path = "here/",
               result = "";

        AWSCredentials creds = new PropertiesCredentials(Worker.class.getResourceAsStream("/AWSCredentials.properties"));

        AmazonSQS sqsTasks = new AmazonSQSClient(creds),
                  sqsFinished = new AmazonSQSClient(creds),
                  sqsLocal = new AmazonSQSClient(creds);

        AmazonS3 s3 = new AmazonS3Client(creds);

        ReceiveMessageRequest req = new ReceiveMessageRequest(tasksUrl);

        List<Message> workerMsgs, localMsgs;
        Message msg;

        // Process messages.
        workerMsgs = getMessages(req, sqsTasks);
        localMsgs = getMessages(req, sqsTasks);
        do {
            // Sleep if no messages arrived, and retry re-fetch new ones afterwards.
            if ((localMsgs == null || localMsgs.size() == 0)
                && (workerMsgs == null || workerMsgs.size() == 0)) {

                logger.info("no messages, sleeping.");

                try {
                    TimeUnit.SECONDS.sleep(5);
                } catch(InterruptedException e) {
                    logger.severe(e.getMessage());
                    result = "shutdown";
                }

            } else {
                // Local messages should be handled before worker messages (higher priority).
                if (localMsgs != null && localMsgs.size() > 0) {

                    msg = localMsgs.get(0);
                    result = handleMessage(msg, s3, bucket, path);
                    if (result != null) {
                        // Only handled messages should be deleted.
                        deleteTaskMessage(msg, localUrl, sqsLocal);
                        if ( ! result.equals("shutdown")) {
                            // TODO reply to local queue.
                            // sendFinishedMessage(msg, result, finishedUrl, sqsFinished);
                        }
                    }
                }

                // TODO handle worker messages as well.
                if (workerMsgs != null && workerMsgs.size() > 0) {
                }

                if (result != null && ! result.equals("shutdown")) {
                    workerMsgs = getMessages(req, sqsTasks);
                    localMsgs = getMessages(req, sqsTasks);
                }
            }

        } while ( result == null || ! result.equals("shutdown"));

        logger.info("shutting down.");
    }
}
