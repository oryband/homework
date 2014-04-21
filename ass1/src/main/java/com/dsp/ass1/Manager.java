package com.dsp.ass1;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.GetQueueAttributesResult;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;


public class Manager {

    private static final Logger logger = Logger.getLogger(Manager.class.getName());
    private static Map < Integer , MyPair> missions; // key is the number of the mission, value is how many task is in the queue
    private static int workerCount = 0 ;
    private static final int tasksForWorker  = 15;
    private static int missionCounter  = 0;


    public static void shutdownWorkers(AmazonSQS sqs, int number) { //TODO handle to time before the worker is terminate
        for (int i = 0 ; i < number ; i++ )
            Utils.sendMessage(sqs, Utils.finishedUrl, "shutdown");
        workerCount -= number;
    }


    public static void creatNewWorkers(int number) { //TODO
    }

    public static String stringAppend(String s,String[] split) {
        return s+"\n<"+split[1]+"> :\t"+split[2]+"\t"+split[3];
    }


    public static void handleFinishQueue(AmazonSQS sqs, AmazonS3 s3) throws IOException {

        List<Message> finishedMsgs = Utils.getMessages(new ReceiveMessageRequest(Utils.finishedUrl), sqs);

        while (!isEmpty(finishedMsgs)){ // action = split[1] , input = split[2], output = split[3], missionNumber = split[4];
            Message msg = finishedMsgs.get(0);
            String[] split = msg.getBody().split("\t");
            int missionNumber = Integer.parseInt(split[4]);
            MyPair pair = missions.get(missionNumber);
            if (split[0].equals("done PDF task")){
                String info = stringAppend(pair.getSecond(),split);
                pair.setSecond(info);
            }
            pair.decrease();
            if (pair.getFirst() == 0){
                String link = Utils.uploadFileToS3(s3, missionNumber + "_results.txt", pair.getSecond());
                if (link == null){
                    Utils.sendMessage(sqs, Utils.localUrl, "Cant upload finish file for mission"+missionNumber);
                }else {
                    finishMission (sqs, missionNumber , link);
                }
                missions.remove(missionNumber);
            } else {
                missions.put(missionNumber,pair);
            }
        }
    }

    public static void finishMission(AmazonSQS sqs, int number, String link) {
        Utils.sendMessage(sqs, Utils.localUrl, "done task\t" + link);
        logger.info("finished mission number : "+number);
        missions.remove(number);
        missionCounter -= 1;
    }


    public static int getNumberOfMessages(AmazonSQS sqs, String url) {
        List <String> attributeNames = new ArrayList<String>();
        attributeNames.add("ApproximateNumberOfMessages");
        GetQueueAttributesResult res = sqs.getQueueAttributes(url, attributeNames);
        return Integer.parseInt(res.getAttributes().get("ApproximateNumberOfMessages"));
    }


    public static void checkWorkerCount(AmazonSQS sqs) {
        int numberOfTasks = getNumberOfMessages(sqs, Utils.tasksUrl);
        if (numberOfTasks < workerCount * tasksForWorker){
            shutdownWorkers(sqs,(int)((workerCount * tasksForWorker) - numberOfTasks) / tasksForWorker);
        } else {
            creatNewWorkers((numberOfTasks - (workerCount * tasksForWorker)) / tasksForWorker);
        }
    }


    public static boolean isEmpty(List<Message> mess1) {
        return ((mess1 == null) || (mess1.size() == 0));
    }


    public static void handleNewTask(String link,AmazonSQS sqs) throws IOException {
        logger.info("handling new task: " + link);
        String info = Utils.LinkToString(link),
                msg;

        if (info == null) {
            Utils.sendMessage(sqs, Utils.localUrl, "Manger cant open file: "+link);
        }

        String[] split = info.split("\n");

        for (int i = 0 ; i < split.length ; i++ ) {
            msg = "new PDF task\t" + info + "\t" + missionCounter ;
            Utils.sendMessage(sqs, Utils.tasksUrl, msg);
        }

        MyPair pair = new MyPair(new Integer (split.length) ,"");
        missions.put(new Integer(missionCounter), pair);
        missionCounter += 1;
    }

    public static String[] getSplit(Message message){
        String body = message.getBody();
        logger.info("received: " + body);
        return body.split("\t");
    }

    public static void main(String[] args) throws InterruptedException,
    IOException {
        Utils.setLogger(logger);

        logger.info("starting.");

        String action,link;

        AWSCredentials creds = Utils.loadCredentials();

        AmazonSQS sqs = new AmazonSQSClient(creds);;

        AmazonS3 s3 = new AmazonS3Client(creds);

        List<Message> localMsgs, TasksMsgs;
        Message msg;

        // Process messages.
        TasksMsgs = Utils.getMessages(new ReceiveMessageRequest(Utils.tasksUrl), sqs);
        localMsgs = Utils.getMessages(new ReceiveMessageRequest(Utils.localUrl), sqs);

        while (true) {
            // Sleep if no messages arrived, and retry to re-fetch new ones afterwards.
            if ((isEmpty(TasksMsgs)) && (isEmpty(localMsgs))) {
                logger.info("no messages, sleeping.");

                try {
                    TimeUnit.SECONDS.sleep(5);
                } catch(InterruptedException e) {
                    logger.severe(e.getMessage());
                    return;
                }

                // Process top message in queue if there are any messages.
            } else if (!isEmpty(localMsgs)) {
                msg = localMsgs.get(0);
                String[] splitMessage = getSplit(msg);
                action = splitMessage[0];
                link = splitMessage[1];
                if (action.equals("new task")) {
                    Utils.deleteTaskMessage(msg, Utils.localUrl, sqs);
                    handleNewTask(link, sqs);
                } else if (action.equals("shutdown")) {
                    Utils.deleteTaskMessage(msg, Utils.localUrl, sqs);
                    break;
                } else {
                    logger.info("ignoring: " + msg.getBody());
                }
            }
            checkWorkerCount(sqs);
            handleFinishQueue(sqs, s3);
            TasksMsgs = Utils.getMessages(new ReceiveMessageRequest(Utils.tasksUrl), sqs);
            localMsgs = Utils.getMessages(new ReceiveMessageRequest(Utils.localUrl), sqs);
        }
        logger.info("finishing.");
    }
}

