package com.dsp.ass1;

import java.util.ArrayList;
import java.util.HashMap;
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
    private static Map < Integer , MissionData> missions; // key is the number of the mission, value is how many task is in the queue
    private static int workerCount = 0 ;
    private static final int tasksForWorker  = 15;
    private static int missionCounter  = 0;


    // closing "number" of workers by sending shutdown messages to shutdown queue.
    private static void shutdownWorkers(AmazonSQS sqs, int number) { //TODO handle to time before the worker is terminate
        /*    for (int i = 0 ; i < number ; i++ ) {
            Utils.sendMessage(sqs, Utils.shutdownUrl, "shutdown");
        }
        workerCount -= number; */
    }


    // create "number" new workers
    private static void creatNewWorkers(int number) {
        //TODO open the workers
        /*   workerCount += number;*/
    }


    // get the approximate number of the messages from the queue with the "url".
    private static int getNumberOfMessages(AmazonSQS sqs, String url) {
        List <String> attributeNames = new ArrayList<String>();
        attributeNames.add("ApproximateNumberOfMessages");
        GetQueueAttributesResult res = sqs.getQueueAttributes(url, attributeNames);
        return Integer.parseInt(res.getAttributes().get("ApproximateNumberOfMessages"));
    }


    // checks the amount of the workers and the amount of the tasks. close or open new workers.
    private static void checkWorkerCount(AmazonSQS sqs) {
        int numberOfTasks = getNumberOfMessages(sqs, Utils.tasksUrl),
                numberOfWorkers = workerCount + getNumberOfMessages(sqs, Utils.finishedUrl);

        if (numberOfTasks < numberOfWorkers * tasksForWorker) {
            int workers2close = (int)((numberOfWorkers * tasksForWorker) - numberOfTasks) / tasksForWorker;
            logger.info("workersCount : got " + numberOfTasks + " tasks and " + numberOfWorkers + " workers. closing " + workers2close + " workers");
            shutdownWorkers(sqs, workers2close);
        }
        else {
            int workers2open = (int)((numberOfTasks - (numberOfWorkers * tasksForWorker)) / tasksForWorker);
            logger.info("workersCount : got " + numberOfTasks + " tasks and " + numberOfWorkers + " workers. open " + workers2open + " workers");
            creatNewWorkers(workers2open);
        }
    }


    // closing all the remain workers and the manager itself.
    private static void closeAll(AmazonSQS sqs) {
        logger.info("starting to close all");
        shutdownWorkers(sqs,workerCount);
        //TODO close myself
    }

    private static void finishMission(AmazonSQS sqs, AmazonS3 s3, int missionNumber, MissionData data){
        logger.info("finishing mission number : " + missionNumber);
        String link = Utils.uploadFileToS3(s3, missionNumber + "_results.txt", Utils.resultPath, data.getInfo());

        if (link == null) {
            Utils.sendMessage(sqs, Utils.localUrl, "Cant upload finish file for mission" + missionNumber);
        }
        else {
            Utils.sendMessage(sqs, Utils.localUrl, "done task\t" + link);
            logger.info("finished mission number : " + missionNumber);
        }

        missions.remove(missionNumber);
        missionCounter -= 1;
    }


    // handling new message in finish queue - msg != null.
    private static void handleFinishTask(Message msg, AmazonSQS sqs, AmazonS3 s3) {
        // action = split[1] , input = split[2], output = split[3], missionNumber = split[4];
        logger.info("got message in finish queue: " + msg.getBody());

        Utils.deleteTaskMessage(msg, Utils.finishedUrl, sqs);
        String[] split = msg.getBody().split("\t");
        int missionNumber = Integer.parseInt(split[4]);
        MissionData data = missions.get(missionNumber);

        if (split[0].equals("done PDF task")) {
            data.stringAppendSucc(split);
        }

        if (split[0].equals("Failed PDF task")) {
            data.stringAppendFailed(split);
        }

        data.decrease();

        if (data.getNumber() == 0){ // finishing mission
            finishMission(sqs, s3, missionNumber , data);
        }
        else {
            missions.put(missionNumber,data);
        }
    }


    // handling new task in local queue.
    private static void handleNewTask(String link,AmazonSQS sqs) {
        logger.info("handling new task: " + link);

        String info = Utils.LinkToString(link),
                msg;

        if (info == null) {
            Utils.sendMessage(sqs, Utils.localUrl, "Manager cant open file: " + link);
            return;
        }

        missionCounter += 1;
        String[] split = info.split("\n");

        for (int i = 0 ; i < split.length ; i++ ) {
            msg = "new PDF task\t" + split[i] + "\t" + missionCounter ;
            Utils.sendMessage(sqs, Utils.tasksUrl, msg);
        }

        MissionData data = new MissionData(split.length);
        missions.put(missionCounter, data);
    }


    // handling new message in the local queue.
    private static String handleLocalTask(Message msg, AmazonSQS sqs) {
        String body = msg.getBody();
        logger.info("received: " + body);
        Utils.deleteTaskMessage(msg, Utils.localUrl, sqs);

        if ((body != null) && body.equals("shutdown")){
            return "shutdown";
        }

        String[] splitMessage = body.split("\t");
        String action = splitMessage[0];
        String link = splitMessage[1];

        if (action.equals("new task")) {
            handleNewTask(link, sqs);
        }
        else {
            logger.info("ignoring: " + msg.getBody());
        }

        return "succ";
    }


    private static void execute(AmazonSQS sqs, AmazonS3 s3) {
        List<Message> localMsgs, finishedMsgs;
        Message msgLocal, msgFinished;
        ReceiveMessageRequest receiveLocal = new ReceiveMessageRequest(Utils.localUrl),
                receiveFinished = new ReceiveMessageRequest(Utils.finishedUrl);

        localMsgs = Utils.getMessages(receiveLocal, sqs);
        finishedMsgs = Utils.getMessages(receiveFinished, sqs);

        boolean gotShutdown = false;

        while ( ! gotShutdown || missions.size() > 0) {

            if (Utils.isEmpty(finishedMsgs) && (gotShutdown || Utils.isEmpty(localMsgs))) {
                logger.info("no messages, sleeping.");

                try {
                    TimeUnit.SECONDS.sleep(5);
                } catch(InterruptedException e) {
                    logger.severe(e.getMessage());
                    return;
                }

            } else {
                if ( ! gotShutdown  &&  ! Utils.isEmpty(localMsgs)) {
                    msgLocal = localMsgs.get(0);

                    if (handleLocalTask(msgLocal, sqs).equals("shutdown")){
                        logger.info("starting shutdown, waiting for all missions to be done");
                        gotShutdown = true;
                    }
                }

                if (!Utils.isEmpty(finishedMsgs)){
                    msgFinished = finishedMsgs.get(0);
                    handleFinishTask(msgFinished, sqs, s3);
                }
            }

            //checkWorkerCount(sqs);

            if ( ! gotShutdown) {
                localMsgs = Utils.getMessages(receiveLocal, sqs);
            }
            finishedMsgs = Utils.getMessages(receiveFinished, sqs);
        }
        logger.info("no missions remain");
    }


    public static void main(String[] args) {
        Utils.setLogger(logger);

        logger.info("starting.");

        AWSCredentials creds = Utils.loadCredentials();
        if(creds == null) {
            logger.severe("Can't find  the credentials file : closing...");
            // TODO close myself
            return;
        }
        //TODO choose mission name
        missions = new HashMap <Integer, MissionData>();

        AmazonSQS sqs = new AmazonSQSClient(creds);

        AmazonS3 s3 = new AmazonS3Client(creds);

        execute(sqs, s3);

        closeAll(sqs);
    }
}