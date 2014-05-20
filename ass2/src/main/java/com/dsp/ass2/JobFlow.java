package com.dsp.ass2;

import java.io.IOException;

import java.util.logging.Logger;

// import org.apache.hadoop.conf.Configuration;
// import org.apache.hadoop.fs.Path;
// import org.apache.hadoop.io.IntWritable;
// import org.apache.hadoop.io.Text;
// import org.apache.hadoop.mapreduce.Job;
// import org.apache.hadoop.mapreduce.Mapper;
// import org.apache.hadoop.mapreduce.Reducer;
// import org.apache.hadoop.mapreduce.Partitioner;
// import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
// import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
// import org.apache.hadoop.io.LongWritable;

import com.amazonaws.auth.AWSCredentials;

import com.amazonaws.services.ec2.model.InstanceType;

import com.amazonaws.services.elasticmapreduce.AmazonElasticMapReduce;
import com.amazonaws.services.elasticmapreduce.AmazonElasticMapReduceClient;
import com.amazonaws.services.elasticmapreduce.model.StepConfig;
import com.amazonaws.services.elasticmapreduce.model.HadoopJarStepConfig;
import com.amazonaws.services.elasticmapreduce.model.JobFlowInstancesConfig;
import com.amazonaws.services.elasticmapreduce.model.RunJobFlowRequest;
import com.amazonaws.services.elasticmapreduce.model.RunJobFlowResult;
import com.amazonaws.services.elasticmapreduce.model.PlacementType;

// import java.io.IOException;

// import java.util.logging.Logger;
import java.util.logging.ConsoleHandler;

import com.amazonaws.auth.PropertiesCredentials;


public class JobFlow {

    private static String countPairsJarUrl = "s3n://yourbucket/yourfile.jar",
            countPairsClass = "CountPairs",
            countPairsInput = "s3n://yourbucket/input/",
            countPairsOutput = "s3n://yourbucket/output/",
            actionOnFailure = "TERMINATE_JOB_FLOW",
            jobName = "jobname",
            hadoopVersion = "2.2.0",
            credentialsPath = "/AWSCredentials.properties",
            ec2KeyName = "ec2.pem",
            placementType = "us-east-1a",
            logUri = "s3n://yourbucket/logs/";

    private static int instanceCount = 2;

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
        AWSCredentials credentials = loadCredentials();
        AmazonElasticMapReduce mapReduce = new AmazonElasticMapReduceClient(credentials);

        HadoopJarStepConfig hadoopJarStep = new HadoopJarStepConfig()
            .withJar(countPairsJarUrl)
            .withMainClass(countPairsClass)
            .withArgs(countPairsInput, countPairsOutput);

        StepConfig stepConfig = new StepConfig()
            .withName("stepname")
            .withHadoopJarStep(hadoopJarStep)
            .withActionOnFailure(actionOnFailure);

        JobFlowInstancesConfig instances = new JobFlowInstancesConfig()
            .withInstanceCount(instanceCount)
            .withMasterInstanceType(InstanceType.M1Small.toString())
            .withSlaveInstanceType(InstanceType.M1Small.toString())
            .withHadoopVersion(hadoopVersion).withEc2KeyName(ec2KeyName)
            .withKeepJobFlowAliveWhenNoSteps(false)
            .withPlacement(new PlacementType(placementType));

        RunJobFlowRequest runFlowRequest = new RunJobFlowRequest()
            .withName(jobName)
            .withInstances(instances)
            .withSteps(stepConfig)
            .withLogUri(logUri);

        RunJobFlowResult runJobFlowResult = mapReduce.runJobFlow(runFlowRequest);
        String jobFlowId = runJobFlowResult.getJobFlowId();
        System.out.println("Ran job flow with id: " + jobFlowId);
    }
}
