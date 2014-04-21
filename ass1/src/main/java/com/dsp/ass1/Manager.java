package com.dsp.ass1;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.ConsoleHandler;
import java.util.logging.Logger;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;

public class Manager {

    private static final Logger logger = Logger.getLogger(Manager.class.getName());

    public static void handleMessage() {

    }

    public static String getAction(Message message){
        String body = message.getBody();
        logger.info("received: " + body);
        return body.split("\t")[0];
    }

    public static void main(String[] args) throws InterruptedException,
            IOException {
        Utils.setLogger(logger);

        logger.info("starting.");

        String action;

        AWSCredentials creds = Utils.loadCredentials();

        AmazonSQS sqsTasks = new AmazonSQSClient(creds),
                sqsFinished = new AmazonSQSClient(creds),
                sqsLocal = new AmazonSQSClient(creds);

        AmazonS3 s3 = new AmazonS3Client(creds);

        ReceiveMessageRequest req = new ReceiveMessageRequest(Utils.tasksUrl);

        List<Message> workerMsgs, localMsgs;
        Message msg;

        // Process messages.
        workerMsgs = Utils.getMessages(req, sqsTasks);
        localMsgs = Utils.getMessages(req, sqsTasks);

        while (true) {
            // Sleep if no messages arrived, and retry to re-fetch new ones afterwards.
            if (localMsgs == null || localMsgs.size() == 0) {
                logger.info("no messages, sleeping.");

                try {
                    TimeUnit.SECONDS.sleep(5);
                } catch(InterruptedException e) {
                    logger.severe(e.getMessage());
                    return;
                }

            // Process top message in queue if there are any messages.
            } else {
                msg = localMsgs.get(0);
                action = getAction(msg);
                if (action.equals("new task")) {
                    Utils.deleteTaskMessage(msg,Utils.localUrl,sqsLocal);
                    handleMessage();
                } else {
                    if (action.equals("shutdown")){
                        Utils.deleteTaskMessage(msg,Utils.localUrl,sqsLocal);
                        break;
                    }
                    else {
                        logger.info("ignoring: " + msg.getBody());
                    }
                }
            }

            localMsgs = Utils.getMessages(req, sqsLocal);
        }
        // Shutting down.
    }
}

