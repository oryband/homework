package com.dsp.ass1;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.apache.commons.io.FilenameUtils;

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
    private static Map < String , MissionData> missions; // key is the number of the mission, value is how many task is in the queue
    private static int workerCount = 0 ;
    private static final int tasksForWorker  = 15;


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
            int workers2close = (int) ((numberOfWorkers * tasksForWorker) - numberOfTasks) / tasksForWorker;
            logger.info("workersCount : got " + numberOfTasks + " tasks and " + numberOfWorkers + " workers. closing " + workers2close + " workers");
            shutdownWorkers(sqs, workers2close);
        } else {
            int workers2open = (int) ((numberOfTasks - (numberOfWorkers * tasksForWorker)) / tasksForWorker);
            logger.info("workersCount : got " + numberOfTasks + " tasks and " + numberOfWorkers + " workers. open " + workers2open + " workers");
            creatNewWorkers(workers2open);
        }
    }


    // closing all the remain workers and the manager itself.
    // TODO send messages to all local apps, so all locals won't wait for DONE msg.
    private static void closeAll(AmazonSQS sqs) {
        logger.info("starting to close all");
        shutdownWorkers(sqs,workerCount);
        //TODO close myself
    }


    private static void finishMission(AmazonSQS sqs, AmazonS3 s3, String missionNumber, MissionData data){
        logger.info("finishing mission number : " + missionNumber);
        String link = Utils.uploadFileToS3(s3, missionNumber + "_results.txt", Utils.resultPath, data.getInfo());

        if (link == null) {
            logger.info("failed task\tCant upload finish file\t" + missionNumber);
            Utils.sendMessage(sqs, Utils.localDownUrl, "failed task\tCant upload finish file\t" + missionNumber);
        }
        else {
            logger.info("done task\t" + link + "\t" + missionNumber);
            Utils.sendMessage(sqs, Utils.localDownUrl, "done task\t" + link + "\t" + missionNumber);
        }

        missions.remove(missionNumber);
    }


    // Handle new worker finish task: Update mission data,
    // and finish its mission if this was its last task.
    private static void finishTask(Message msg, AmazonSQS sqs, AmazonS3 s3) {
        Utils.deleteTaskMessage(msg, Utils.finishedUrl, sqs);

        String body = msg.getBody();
        if (body == null) {
            logger.severe("Error in message received, body is null.");
            return;
        }

        logger.info("Finish message: " + msg.getBody());

        // action = split[1] , input = split[2], output = split[3], missionNumber = split[4];
        String[] split = msg.getBody().split("\t");
        if (split.length < 5) {
            logger.severe("Finished task message is too short < 5.");
            return;
        }

        String missionNumber = split[4];
        MissionData data = missions.get(missionNumber);

        if (split[0].equals("done PDF task")) {
            data.appendSucc(split);
        } else if (split[0].equals("failed PDF task")) {
            data.appendFailed(split);
        } else {
            logger.severe("Received unknown message.");
            return;
        }

        data.decrease();

        // Finish mission if all its tasks were finished.
        if (data.getNumber() == 0) {
            finishMission(sqs, s3, missionNumber , data);
        } else {
            missions.put(missionNumber,data);
        }
    }


    // Receives an S3 mission link 'http://xxx/mission-id_input.txt'
    // and returns 'mission-id'.
    private static String getMissionNumber(String link) {
        String base = FilenameUtils.getBaseName(link);
        return base.split("_")[0];
    }


    // Process a new local task message, by adding PDF tasks to worker queue,
    // and launching/terminating worker instances as necessarProcess a new
    // local task message, by adding PDF tasks to worker queue.
    private static void handleNewMission(String link, AmazonSQS sqs) {
        logger.info("handling new task: " + link);

        String info = Utils.LinkToString(link),
               missionNumber = getMissionNumber(link),
               msg;

        // Don't do anything if any error occured.
        if (info == null) {
            Utils.sendMessage(sqs, Utils.localDownUrl, "failed task\tCan't open the link\t" + missionNumber);
            return;
        }

        String[] tasks = info.split("\n");

        // Add PDF tasks to queue.
        logger.info("Adding new mission's PDF tasks to worker queue.");
        for (int i = 0; i < tasks.length; i++) {
            msg = "new PDF task\t" + tasks[i] + "\t" + missionNumber ;
            Utils.sendMessage(sqs, Utils.tasksUrl, msg);
        }

        // Create new missions data object.
        MissionData data = new MissionData(tasks.length);
        missions.put(missionNumber, data);
    }


    // Creates a new mission (received from local) or shuts down.
    // Returns 'true' if needs to shutdown afterwards, 'false' otherwise.
    private static boolean handleLocalMessage(Message msg, AmazonSQS sqs) {
        Utils.deleteTaskMessage(msg, Utils.localUpUrl, sqs);

        String body = msg.getBody();
        // Delete processed message.
        if (body == null) {
            logger.severe("Error in message received, body is null.");
            return false;
        }

        logger.info("Received: " + body);


        // Shutdown if 'shutdown' message received.
        if (body.equals("shutdown")) {
            return true;
        }

        // Else process new mission.
        String[] mission = body.split("\t");
        if (mission.length >= 2 && mission[0].equals("new task")) {
            handleNewMission(mission[1], sqs);  // mission[1] is a link.
        } else {
            logger.info("Ignoring: " + msg.getBody());
        }

        return false;
    }


    private static void execute(AmazonSQS sqs, AmazonS3 s3) {
        boolean shutdown = false;
        List<Message> localUpMsgs, finishedMsgs;
        Message msg;
        ReceiveMessageRequest rcvLocalUp = new ReceiveMessageRequest(Utils.localUpUrl),
                              rcvFinished = new ReceiveMessageRequest(Utils.finishedUrl);

        localUpMsgs = Utils.getMessages(rcvLocalUp, sqs);
        finishedMsgs = Utils.getMessages(rcvFinished, sqs);

        // Process new missions as long as there are any missions left,
        // and shutdown flag is OFF.
        while (missions.size() > 0 || ! shutdown) {

            // Sleep if no messages were received.
            // Don't accept new missions if shutdown flag is ON.
            if (Utils.isEmpty(finishedMsgs)
                    && (shutdown || Utils.isEmpty(localUpMsgs))) {
                logger.info("no messages, sleeping.");

                try {
                    TimeUnit.SECONDS.sleep(5);
                } catch(InterruptedException e) {
                    logger.severe(e.getMessage());
                    return;
                }
            } else {
                // Process new mission, if shutdown is OFF.
                if ( ! shutdown && ! Utils.isEmpty(localUpMsgs)) {
                    msg = localUpMsgs.get(0);
                    shutdown = handleLocalMessage(msg, sqs);
                    if (shutdown) {
                        logger.info("Shutting down.");
                    }
                }

                // Process new worker finished task.
                if ( ! Utils.isEmpty(finishedMsgs)){
                    msg = finishedMsgs.get(0);
                    finishTask(msg, sqs, s3);
                }
            }

            //checkWorkerCount(sqs);  // TODO

            // Fetch more local missions if shutdown flag is OFF.
            if ( ! shutdown) {
                localUpMsgs = Utils.getMessages(rcvLocalUp, sqs);
            }

            // Fetch remaining worker finish tasks.
            finishedMsgs = Utils.getMessages(rcvFinished, sqs);
        }

        logger.info("No more missions.");
    }


    public static void main(String[] args) {
        Utils.setLogger(logger);

        logger.info("Starting.");

        AWSCredentials creds = Utils.loadCredentials();
        if(creds == null) {
            logger.severe("Couldn't load credentials.");
            // TODO close myself
            return;
        }

        missions = new HashMap <String, MissionData>();

        // Start S3 and SQS missions.
        AmazonSQS sqs = new AmazonSQSClient(creds);
        AmazonS3 s3 = new AmazonS3Client(creds);

        execute(sqs, s3);
        closeAll(sqs);
    }
}
