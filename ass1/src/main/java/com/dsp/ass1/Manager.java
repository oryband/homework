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


    // closing "number" of workers by sending shutdown messages to shutdown queue.
    private static void shutdownWorkers(AmazonSQS sqs, int number) { //TODO handle to time before the worker is terminate
        for (int i = 0 ; i < number ; i++ ) {
            Utils.sendMessage(sqs, Utils.shutdownUrl, "shutdown");
        }
        workerCount -= number;
    }


    // create "number" new workers
    private static void creatNewWorkers(int number) {
        //TODO open the workers
        workerCount += number;
    }


    // create the line for each pdf in the results file.
    private static String stringAppend(String s,String[] split) {
        return s + "\n<" + split[1] + "> :\t" + split[2] + "\t" + split[3];
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
        int numberOfTasks = getNumberOfMessages(sqs, Utils.tasksUrl);

        if (numberOfTasks < workerCount * tasksForWorker) {
            int workers2close = (int)((workerCount * tasksForWorker) - numberOfTasks) / tasksForWorker;
            logger.info("workersCount : got " + numberOfTasks + "tasks and " + "workerCount" + "workers. closing " + workers2close + "workers");
            shutdownWorkers(sqs, workers2close);
        }
        else {
            int workers2open = (int)((numberOfTasks - (workerCount * tasksForWorker)) / tasksForWorker);
            logger.info("workersCount : got " + numberOfTasks + "tasks and " + "workerCount" + "workers. open " + workers2open + "workers");
            creatNewWorkers(workers2open);
        }
    }


    // closing all the remain workers and the manager itself.
    private static void closeAll(AmazonSQS sqs) {
        logger.info("starting to close all");
        shutdownWorkers(sqs,workerCount);
        //TODO close myself
    }


    // handling new message in finish queue - msg != null.
    private static void handleFinishTask(Message msg, AmazonSQS sqs, AmazonS3 s3) throws IOException {
        // action = split[1] , input = split[2], output = split[3], missionNumber = split[4];
        logger.info("got message in finish queue: " + msg.getBody());

        Utils.deleteTaskMessage(msg, Utils.finishedUrl, sqs);
        String[] split = msg.getBody().split("\t");
        int missionNumber = Integer.parseInt(split[4]);
        MyPair pair = missions.get(missionNumber);

        if (split[0].equals("done PDF task")) { // the other case is an error message
            String info = stringAppend(pair.getSecond(),split);
            pair.setSecond(info);
        }

        pair.decrease();

        if (pair.getFirst() == 0){ // finishing mission
            logger.info("finishing mission number : " + missionNumber);
            String link = Utils.uploadFileToS3(s3, missionNumber + "_results.txt", pair.getSecond());

            if (link == null){
                Utils.sendMessage(sqs, Utils.localUrl, "Cant upload finish file for mission" + missionNumber);
            }
            else {
                Utils.sendMessage(sqs, Utils.localUrl, "done task\t" + link);
                logger.info("finished mission number : " + missionNumber);
            }

            missions.remove(missionNumber);
            missionCounter -= 1;
        }
        else {
            missions.put(missionNumber,pair);
        }
    }


    // handling new task in local queue.
    private static void handleNewTask(String link,AmazonSQS sqs) throws IOException {
        logger.info("handling new task: " + link);

        String info = Utils.LinkToString(link),
                msg;

        if (info == null) {
            Utils.sendMessage(sqs, Utils.localUrl, "Manger cant open file: " + link);
            return;
        }

        missionCounter += 1;
        String[] split = info.split("\n");

        for (int i = 0 ; i < split.length ; i++ ) {
            msg = "new PDF task\t" + info + "\t" + missionCounter ;
            Utils.sendMessage(sqs, Utils.tasksUrl, msg);
        }

        MyPair pair = new MyPair(split.length, "");
        missions.put(missionCounter, pair);
    }


    // handling new message in the local queue.
    private static String handleLocalTask(Message msg, AmazonSQS sqs) throws IOException {
        String body = msg.getBody();
        String[] splitMessage = body.split("\t");
        String action = splitMessage[0];
        String link = splitMessage[1];
        logger.info("received: " + body);

        if (action.equals("new task")) {
            Utils.deleteTaskMessage(msg, Utils.localUrl, sqs);
            handleNewTask(link, sqs);
        }
        else {
            if (action.equals("shutdown")) {
                Utils.deleteTaskMessage(msg, Utils.localUrl, sqs);
                return "shutdown";
            }
            else {
                logger.info("ignoring: " + msg.getBody());
            }
        }

        return "succ";
    }


    public static void main(String[] args) throws InterruptedException,
    IOException {
        Utils.setLogger(logger);

        logger.info("starting.");

        AWSCredentials creds = Utils.loadCredentials();
        if(creds == null) {
            logger.severe("Can't find  the credentials file : closing...");
            // TODO close myself
            return;
        }

        AmazonSQS sqs = new AmazonSQSClient(creds);

        AmazonS3 s3 = new AmazonS3Client(creds);

        List<Message> localMsgs, finishedMsgs;
        Message msgLocal, msgFinished;
        ReceiveMessageRequest receiveLocal = new ReceiveMessageRequest(Utils.localUrl),
                              receiveFinished = new ReceiveMessageRequest(Utils.finishedUrl);

        localMsgs = Utils.getMessages(receiveLocal, sqs);
        finishedMsgs = Utils.getMessages(receiveFinished, sqs);

        // before shutdown received
        while (true) {

            if (Utils.isEmpty(localMsgs) && Utils.isEmpty(finishedMsgs)) {
                logger.info("no messages, sleeping.");

                try {
                    TimeUnit.SECONDS.sleep(5);
                } catch(InterruptedException e) {
                    logger.severe(e.getMessage());
                    closeAll(sqs);
                    return;
                }

            } else {
                if (!Utils.isEmpty(localMsgs)) {
                    msgLocal = localMsgs.get(0);

                    if (handleLocalTask(msgLocal, sqs).equals("shutdown")){
                        break;
                    }
                }

                if (!Utils.isEmpty(finishedMsgs)){
                    msgFinished = finishedMsgs.get(0);
                    handleFinishTask(msgFinished, sqs, s3);
                }
            }

            checkWorkerCount(sqs);
            localMsgs = Utils.getMessages(receiveLocal, sqs);
            finishedMsgs = Utils.getMessages(receiveFinished, sqs);
        }

        // after shutdown received but tasks queue has messages
        logger.info("starting shutdown");

        while (getNumberOfMessages(sqs,Utils.tasksUrl) > 0) {
            logger.info("tasksQueue still has messages, sleeping.");

            try {
                TimeUnit.SECONDS.sleep(5);
            } catch(InterruptedException e) {
                logger.severe(e.getMessage());
                closeAll(sqs);
                return;
            }

            checkWorkerCount(sqs);
            finishedMsgs = Utils.getMessages(receiveFinished, sqs);

            if (!Utils.isEmpty(finishedMsgs)){
                msgFinished = finishedMsgs.get(0);
                handleFinishTask(msgFinished, sqs, s3);
            }
        }

        // after tasks queue has no messages but finished queue has.
        logger.info("no messages in tasks queue");
        finishedMsgs = Utils.getMessages(receiveFinished, sqs);

        while (!Utils.isEmpty(finishedMsgs)) {
            // TODO check if checkWorkerCount(sqs) should be here;
            msgFinished = finishedMsgs.get(0);
            handleFinishTask(msgFinished, sqs, s3);
        }

        // starting to close all.
        logger.info("no messages in finished queue");
        closeAll(sqs);
    }
}