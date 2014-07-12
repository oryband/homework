package com.dsp.ass3;

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

    private static String actionOnFailure = "TERMINATE_JOB_FLOW",
            jobName = "jobname",

            ec2KeyName = "ec2",
            placementType = "us-east-1a",
            amiVersion = "2.4.2",
            // amiVersion = "3.1.0",
            hadoopVersion = "1.0.3",
            // hadoopVersion = "2.4.0",
            instanceType = InstanceType.M1Small.toString(),
            // instanceType = InstanceType.M1Xlarge.toString(),

            s3BaseUri = "s3://" + Utils.bucket + "/",

            logUri = s3BaseUri + "logs/",

            hadoopOutputFileName = "part-r-*",

            pairsClass = "Pairs",
            biarcsClass = "Biarcs",
            joinClass = "Join",
            labelsClass = "Labels",
            headersClass = "Headers",
            singleLineClass = "SingleLine",

            pairsJarUrl = s3BaseUri + "jars/Pairs.jar",
            biarcsJarUrl = s3BaseUri + "jars/Biarcs.jar",
            joinJarUrl = s3BaseUri + "jars/Join.jar",
            labelsJarUrl = s3BaseUri + "jars/labels.jar",
            headersJarUrl = s3BaseUri + "jars/headers.jar",
            singleLineJarUrl = s3BaseUri + "jars/SingleLine.jar",

            pairsOutput =  s3BaseUri + "steps/pairs/output/",
            biarcsOutput = s3BaseUri +  "steps/biarcs/output/",
            joinOutput = s3BaseUri + "steps/join/output/",
            labelsOutput = s3BaseUri + "steps/labels/output/",
            headersOutput = s3BaseUri + "steps/headers/output/",
            singleLineOutput = s3BaseUri + "steps/singleLine/output/",

            pairsInput = s3BaseUri + "steps/pairs/input/hypernym.txt",
            biarcsInputPrefix = "s3://bgudsp142/syntactic-ngram/biarcs/biarcs.",
            joinInput1 = pairsOutput + hadoopOutputFileName,
            joinInput2 = biarcsOutput + hadoopOutputFileName,
            labelsInput = joinOutput + hadoopOutputFileName,
            headersInput = labelsOutput + hadoopOutputFileName,
            singleLineInput = joinOutput + hadoopOutputFileName;


    public static void main(String[] args) throws Exception {
        // Load credentials.
        AWSCredentials credentials = Utils.loadCredentials();
        AmazonElasticMapReduce mapReduce = new AmazonElasticMapReduceClient(credentials);

        // Set Pairs job flow step.
        HadoopJarStepConfig pairsJarConfig = new HadoopJarStepConfig()
            .withJar(pairsJarUrl)
            .withMainClass(pairsClass)
            .withArgs(pairsInput, pairsOutput);

        StepConfig pairsConfig = new StepConfig()
            .withName(pairsClass)
            .withHadoopJarStep(pairsJarConfig)
            .withActionOnFailure(actionOnFailure);

        // Set Biarcs job flow step.
        HadoopJarStepConfig biarcsJarConfig = new HadoopJarStepConfig()
            .withJar(biarcsJarUrl)
            .withMainClass(biarcsClass)
            .withArgs(biarcsInputPrefix, biarcsOutput);

        StepConfig biarcsConfig = new StepConfig()
            .withName(biarcsClass)
            .withHadoopJarStep(biarcsJarConfig)
            .withActionOnFailure(actionOnFailure);

        // Set Join job flow step.
        HadoopJarStepConfig joinJarConfig = new HadoopJarStepConfig()
            .withJar(joinJarUrl)
            .withMainClass(joinClass)
            // NOTE we JOIN the output of the two previous steps.
            .withArgs(joinInput1, joinInput2, joinOutput);

        StepConfig joinConfig = new StepConfig()
            .withName(joinClass)
            .withHadoopJarStep(joinJarConfig)
            .withActionOnFailure(actionOnFailure);

        // Set Labels job flow step.
        HadoopJarStepConfig labelsJarConfig = new HadoopJarStepConfig()
            .withJar(labelsJarUrl)
            .withMainClass(labelsClass)
            .withArgs(labelsInput, labelsOutput);

        StepConfig labelsConfig = new StepConfig()
            .withName(labelsClass)
            .withHadoopJarStep(labelsJarConfig)
            .withActionOnFailure(actionOnFailure);

        // Set Headers job flow step.
        HadoopJarStepConfig headersJarConfig = new HadoopJarStepConfig()
            .withJar(headersJarUrl)
            .withMainClass(headersClass)
            .withArgs(headersInput, headersOutput);

        StepConfig headersConfig = new StepConfig()
            .withName(headersClass)
            .withHadoopJarStep(headersJarConfig)
            .withActionOnFailure(actionOnFailure);

        // Set SingleLine job flow step.
        HadoopJarStepConfig singleLineJarConfig = new HadoopJarStepConfig()
            .withJar(singleLineJarUrl)
            .withMainClass(singleLineClass)
            .withArgs(singleLineInput, singleLineOutput);

        StepConfig singleLineConfig = new StepConfig()
            .withName(singleLineClass)
            .withHadoopJarStep(singleLineJarConfig)
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
            .withLogUri(logUri)
            // .withSteps(pairsConfig, biarcsConfig, joinConfig, labelsConfig, headersClass, singleLineConfig);
            // Custom steps.
            .withSteps(joinConfig);

        // Execute job flow.
        RunJobFlowResult runJobFlowResult = mapReduce.runJobFlow(runFlowRequest);
        String jobFlowId = runJobFlowResult.getJobFlowId();
        System.out.println("ID: " + jobFlowId);
    }
}
