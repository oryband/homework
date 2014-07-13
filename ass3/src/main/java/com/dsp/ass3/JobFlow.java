package com.dsp.ass3;

import java.util.Arrays;
import java.util.logging.Logger;

import com.amazonaws.auth.AWSCredentials;
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

    private static final Logger logger = Utils.setLogger(Logger.getLogger(JobFlow.class.getName()));

    private static int instanceCount = 3;

    private static String
        actionOnFailure = "TERMINATE_JOB_FLOW",
        jobName = "jobname",

        ec2KeyName = "ec2",
        placementType = "us-east-1a",
        amiVersion = "2.4.2",
        hadoopVersion = "1.0.3",
        instanceType = InstanceType.M1Small.toString(),
        // instanceType = InstanceType.M1Xlarge.toString(),

        s3BaseUri = "s3://" + Utils.bucket + "/",

        logUri = s3BaseUri + "logs/",

        hadoopOutputFileName = "part-r-*",

        pairsClass = "Pairs",
        biarcsClass = "Biarcs",
        joinClass = "Join",
        sumClass = "Sum",
        labelsClass = "Labels",
        headersClass = "Headers",
        singleLineClass = "SingleLine",
        vectorsClass = "Vectors",

        pairsJarUrl = s3BaseUri + "jars/Pairs.jar",
        biarcsJarUrl = s3BaseUri + "jars/Biarcs.jar",
        joinJarUrl = s3BaseUri + "jars/Join.jar",
        sumJarUrl = s3BaseUri + "jars/sum.jar",
        labelsJarUrl = s3BaseUri + "jars/labels.jar",
        headersJarUrl = s3BaseUri + "jars/headers.jar",
        singleLineJarUrl = s3BaseUri + "jars/SingleLine.jar",
        vectorsJarUrl = s3BaseUri + "jars/Vectors.jar",

        pairsOutput =  s3BaseUri + "steps/pairs/output/",
        biarcsOutput = s3BaseUri +  "steps/biarcs/output/",
        joinOutput = s3BaseUri + "steps/join/output/",
        sumOutput = s3BaseUri + "steps/sum/output/",
        labelsOutput = s3BaseUri + "steps/labels/output/",
        headersOutput = s3BaseUri + "steps/headers/output/",
        singleLineOutput = s3BaseUri + "steps/singleLine/output/",
        vectorsOutput = s3BaseUri + "steps/vectors/output/",

        pairsInput = s3BaseUri + "steps/pairs/input/hypernym.txt",
        biarcsInputPrefix = "s3://bgudsp142/syntactic-ngram/biarcs/biarcs.",
        joinInput1 = pairsOutput + hadoopOutputFileName,
        joinInput2 = biarcsOutput + hadoopOutputFileName,
        sumInput = joinOutput + hadoopOutputFileName,
        labelsInput = sumOutput + hadoopOutputFileName,
        headersInput = labelsOutput + hadoopOutputFileName,
        singleLineInput = sumOutput + hadoopOutputFileName,
        vectorsInput1 = "steps/labels/output/labels",
        vectorsInput2 = singleLineOutput + hadoopOutputFileName;


    private static HadoopJarStepConfig createJarStepConfig(String jar, String cls, String... args) {
        return new HadoopJarStepConfig()
            .withJar(jar)
            .withMainClass(cls)
            .withArgs(Arrays.asList(args));
    }


    private static StepConfig createStepConfig(String cls, HadoopJarStepConfig stepConfig) {
        return new StepConfig()
            .withName(cls)
            .withHadoopJarStep(stepConfig)
            .withActionOnFailure(actionOnFailure);
    }


    // Create instances configuration.
    private static JobFlowInstancesConfig createJobFlowInstancesConfig() {
        return new JobFlowInstancesConfig()
            .withInstanceCount(instanceCount)
            .withMasterInstanceType(instanceType)
            .withSlaveInstanceType(instanceType)
            .withHadoopVersion(hadoopVersion)
            .withEc2KeyName(ec2KeyName)
            .withKeepJobFlowAliveWhenNoSteps(false)
            .withPlacement(new PlacementType(placementType));
    }

    // Create job flow request.
    private static RunJobFlowRequest createRunJobFlowRequest(JobFlowInstancesConfig instances, StepConfig... steps) {
        return new RunJobFlowRequest()
            .withName(jobName)
            .withAmiVersion(amiVersion)
            .withInstances(instances)
            .withLogUri(logUri)
            .withSteps(Arrays.asList(steps));
    }



    public static void main(String[] args) throws Exception {
        // Load credentials and init AWS EMR client.
        AWSCredentials credentials = Utils.loadCredentials();
        AmazonElasticMapReduce mapReduce = new AmazonElasticMapReduceClient(credentials);

        StepConfig
            pairsConfig = createStepConfig(pairsClass, createJarStepConfig(pairsJarUrl, pairsClass, pairsInput, pairsOutput)),
            biarcsConfig = createStepConfig(biarcsClass, createJarStepConfig(biarcsJarUrl, biarcsClass, biarcsInputPrefix, biarcsOutput)),
            joinConfig = createStepConfig(joinClass, createJarStepConfig(joinJarUrl, joinClass, joinInput1, joinInput2, joinOutput)),
            sumConfig = createStepConfig(sumClass, createJarStepConfig(sumJarUrl, sumClass, sumInput, sumOutput)),
            labelsConfig = createStepConfig(labelsClass, createJarStepConfig(labelsJarUrl, labelsClass, labelsInput, labelsOutput)),
            headersConfig = createStepConfig(headersClass, createJarStepConfig(headersJarUrl, headersClass, headersInput, headersOutput)),
            singleLineConfig = createStepConfig(singleLineClass, createJarStepConfig(singleLineJarUrl, singleLineClass, singleLineInput, singleLineOutput)),
            vectorsConfig = createStepConfig(vectorsClass, createJarStepConfig(vectorsJarUrl, vectorsClass, vectorsInput1, vectorsInput2, vectorsOutput));

        JobFlowInstancesConfig instances = createJobFlowInstancesConfig();

        // Set job flow request.
        RunJobFlowRequest runFlowRequest = createRunJobFlowRequest(instances,
                pairsConfig, biarcsConfig, joinConfig, sumConfig,
                labelsConfig, headersConfig,
                singleLineConfig, vectorsConfig);

        // Custom steps.
        // RunJobFlowRequest runFlowRequest = createRunJobFlowRequest(instances, joinConfig);

        // Execute job flow.
        RunJobFlowResult runJobFlowResult = mapReduce.runJobFlow(runFlowRequest);
        String jobFlowId = runJobFlowResult.getJobFlowId();
        System.out.println("ID: " + jobFlowId);
    }
}
