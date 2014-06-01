package com.dsp.ass2;

import java.io.IOException;
import java.util.logging.ConsoleHandler;
import java.util.logging.Logger;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.ec2.model.InstanceType;
import com.amazonaws.services.elasticmapreduce.AmazonElasticMapReduce;
import com.amazonaws.services.elasticmapreduce.AmazonElasticMapReduceClient;
import com.amazonaws.services.elasticmapreduce.model.HadoopJarStepConfig;
import com.amazonaws.services.elasticmapreduce.model.JobFlowInstancesConfig;
import com.amazonaws.services.elasticmapreduce.model.PlacementType;
import com.amazonaws.services.elasticmapreduce.model.RunJobFlowRequest;
import com.amazonaws.services.elasticmapreduce.model.RunJobFlowResult;
import com.amazonaws.services.elasticmapreduce.model.StepConfig;


public class JobFlow {

    private static String credentialsPath = "/AWSCredentials.properties",

            actionOnFailure = "TERMINATE_JOB_FLOW",
            jobName = "jobname",

            ec2KeyName = "ec2",
            placementType = "us-east-1a",
            amiVersion = "2.4.2",
            hadoopVersion = "1.0.3",
            instanceType = InstanceType.M1Small.toString(),

            logUri = "s3n://ory-dsp-ass2/logs/",

            countPairsClass = "WordCount",
            countPairsJarUrl = "s3n://ory-dsp-ass2/jars/WordCount.jar",
            countPairsInput = "s3n://ory-dsp-ass2/steps/WordCount/input/in",
            countPairsOutput = "s3n://ory-dsp-ass2/steps/WordCount/output/";

    private static int instanceCount = 1;

    private static final Logger logger = setLogger(Logger.getLogger(JobFlow.class.getName()));


    // Use custom string format for logger.
    public static Logger setLogger(Logger logger) {
        ShortFormatter formatter = new ShortFormatter();
        ConsoleHandler handler = new ConsoleHandler();

        logger.setUseParentHandlers(false);
        handler.setFormatter(formatter);
        logger.addHandler(handler);

        return logger;
    }


    public static PropertiesCredentials loadCredentials() {
        try {
            return new PropertiesCredentials(
                    JobFlow.class.getResourceAsStream(credentialsPath));
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }
    }


    public static void main(String[] args) throws Exception {
        // Load credentials.
        AWSCredentials credentials = loadCredentials();
        AmazonElasticMapReduce mapReduce = new AmazonElasticMapReduceClient(credentials);

        // Set WordCount job flow step.
        HadoopJarStepConfig hadoopJarStep = new HadoopJarStepConfig()
            .withJar(countPairsJarUrl)
            .withMainClass(countPairsClass)
            .withArgs(countPairsInput, countPairsOutput);

        StepConfig stepConfig = new StepConfig()
            .withName("stepname")
            .withHadoopJarStep(hadoopJarStep)
            .withActionOnFailure(actionOnFailure);

        // Set instances.
        JobFlowInstancesConfig instances = new JobFlowInstancesConfig()
            .withInstanceCount(instanceCount)
            .withMasterInstanceType(instanceType)
            .withSlaveInstanceType(instanceType)
            .withHadoopVersion(hadoopVersion)
            .withEc2KeyName(ec2KeyName)
            .withKeepJobFlowAliveWhenNoSteps(false)
            .withPlacement(new PlacementType(placementType));

        // Set job flow request.
        RunJobFlowRequest runFlowRequest = new RunJobFlowRequest()
            .withName(jobName)
            .withAmiVersion(amiVersion)
            .withInstances(instances)
            .withSteps(stepConfig)
            .withLogUri(logUri);

        // Execute job flow.
        RunJobFlowResult runJobFlowResult = mapReduce.runJobFlow(runFlowRequest);
        String jobFlowId = runJobFlowResult.getJobFlowId();
        System.out.println("Ran job flow with id: " + jobFlowId);
    }
}
