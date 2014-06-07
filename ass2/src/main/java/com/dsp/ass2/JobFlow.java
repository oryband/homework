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

            // Steps constants.
            countClass = "Count",
            countJarUrl = "s3n://ory-dsp-ass2/jars/Count.jar",
            countInput = "s3n://ory-dsp-ass2/steps/Count/input/eng.corp.10k",
            countOutput = "s3n://ory-dsp-ass2/steps/Count/output/",

            joinClass = "Join",
            joinJarUrl = "s3n://ory-dsp-ass2/jars/Join.jar",
            joinInput = "s3n://ory-dsp-ass2/steps/Count/output/part-r-00000",
            joinOutput = "s3n://ory-dsp-ass2/steps/Join/output/";

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

        // Set Count job flow step.
        HadoopJarStepConfig countJarConfig = new HadoopJarStepConfig()
            .withJar(countJarUrl)
            .withMainClass(countClass)
            .withArgs(countInput, countOutput);

        StepConfig countConfig = new StepConfig()
            .withName("Count")
            .withHadoopJarStep(countJarConfig)
            .withActionOnFailure(actionOnFailure);

        // Set Join job flow step.
        HadoopJarStepConfig joinJarConfig = new HadoopJarStepConfig()
            .withJar(joinJarUrl)
            .withMainClass(joinClass)
            .withArgs(joinInput, joinOutput);

        StepConfig joinConfig = new StepConfig()
            .withName("Join")
            .withHadoopJarStep(joinJarConfig)
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
            .withSteps(countConfig, joinConfig)
            .withLogUri(logUri);

        // Execute job flow.
        RunJobFlowResult runJobFlowResult = mapReduce.runJobFlow(runFlowRequest);
        String jobFlowId = runJobFlowResult.getJobFlowId();
        System.out.println("Ran job flow with id: " + jobFlowId);
    }
}
