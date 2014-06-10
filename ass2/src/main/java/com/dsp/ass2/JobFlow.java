package com.dsp.ass2;

import java.util.logging.Logger;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.ec2.model.InstanceType;
import com.amazonaws.services.elasticmapreduce.AmazonElasticMapReduce;
import com.amazonaws.services.elasticmapreduce.AmazonElasticMapReduceClient;
import com.amazonaws.services.elasticmapreduce.model.BootstrapActionConfig;
import com.amazonaws.services.elasticmapreduce.model.HadoopJarStepConfig;
import com.amazonaws.services.elasticmapreduce.model.JobFlowInstancesConfig;
import com.amazonaws.services.elasticmapreduce.model.PlacementType;
import com.amazonaws.services.elasticmapreduce.model.RunJobFlowRequest;
import com.amazonaws.services.elasticmapreduce.model.RunJobFlowResult;
import com.amazonaws.services.elasticmapreduce.model.ScriptBootstrapActionConfig;
import com.amazonaws.services.elasticmapreduce.model.StepConfig;


public class JobFlow {

    private static final Logger logger = Utils.setLogger(Logger.getLogger(JobFlow.class.getName()));

    private static String actionOnFailure = "TERMINATE_JOB_FLOW",
            jobName = "jobname",

            ec2KeyName = "ec2",
            placementType = "us-east-1a",
            amiVersion = "2.4.2",
            // amiVersion = "3.1.0",
            hadoopVersion = "1.0.3",
            // hadoopVersion = "2.4.0",
            // instanceType = InstanceType.M1Small.toString(),
            instanceType = InstanceType.M1Xlarge.toString(),

            s3BaseUri = "s3n://" + Utils.bucket + "/",

            logUri = s3BaseUri + "logs/",

            updateLuceneUri = s3BaseUri + "lucene/update-lucene.sh",

            hadoopOutputFileName = "part-r-*",

            joinClass = "Join",
            countClass = "Count",
            calculateClass = "Calculate",
            lastDecadeClass = "LastDecade",
            fmeasureClass = "FMeasure",

            countJarUrl = s3BaseUri + "jars/Count.jar",
            joinJarUrl = s3BaseUri + "jars/Join.jar",
            calculateJarUrl = s3BaseUri + "jars/Calculate.jar",
            lastDecadeJarUrl = s3BaseUri + "jars/LastDecade.jar",
            fmeasureJarUrl = s3BaseUri + "jars/FMeasure.jar",

            countOutput = s3BaseUri + Utils.countOutput,
            joinOutput = s3BaseUri + Utils.joinOutput,
            calculateOutput = s3BaseUri + Utils.calculateOutput,
            lastDecadeOutput = s3BaseUri + Utils.lastDecadeOutput,
            fmeasureOutput = s3BaseUri + Utils.fmeasureOutput,

            // countInput = s3BaseUri + "steps/Count/input/eng.corp.10k",  // For Testing.
            countInput = "s3://datasets.elasticmapreduce/ngrams/books/20090715/eng-gb-all/5gram/data",
            joinInput = s3BaseUri + Utils.countOutput + hadoopOutputFileName,
            calculateInput = s3BaseUri + Utils.joinOutput + hadoopOutputFileName,
            lastDecadeInput = s3BaseUri + Utils.joinOutput + hadoopOutputFileName,
            fmeasureInput = s3BaseUri + Utils.lastDecadeOutput + hadoopOutputFileName;

    private static int instanceCount = 12;


    public static void main(String[] args) throws Exception {
        // Load credentials.
        AWSCredentials credentials = Utils.loadCredentials();
        AmazonElasticMapReduce mapReduce = new AmazonElasticMapReduceClient(credentials);

        // Set Count job flow step.
        HadoopJarStepConfig countJarConfig = new HadoopJarStepConfig()
            .withJar(countJarUrl)
            .withMainClass(countClass)
            .withArgs(countInput, countOutput);

        StepConfig countConfig = new StepConfig()
            .withName(countClass)
            .withHadoopJarStep(countJarConfig)
            .withActionOnFailure(actionOnFailure);

        // Set Join job flow step.
        HadoopJarStepConfig joinJarConfig = new HadoopJarStepConfig()
            .withJar(joinJarUrl)
            .withMainClass(joinClass)
            .withArgs(joinInput, joinOutput);

        StepConfig joinConfig = new StepConfig()
            .withName(joinClass)
            .withHadoopJarStep(joinJarConfig)
            .withActionOnFailure(actionOnFailure);

        // Set Calculate job flow step.
        HadoopJarStepConfig calculateJarConfig = new HadoopJarStepConfig()
            .withJar(calculateJarUrl)
            .withMainClass(calculateClass)
            // args[0] is how many top PMI pairs to display.
            .withArgs(calculateInput, calculateOutput, args[0]);

        StepConfig calculateConfig = new StepConfig()
            .withName(calculateClass)
            .withHadoopJarStep(calculateJarConfig)
            .withActionOnFailure(actionOnFailure);

        // Set LastDecade job fow step
        HadoopJarStepConfig lastDecadeJarConfig = new HadoopJarStepConfig()
            .withJar(lastDecadeJarUrl)
            .withMainClass(lastDecadeClass)
            .withArgs(lastDecadeInput, lastDecadeOutput);

        StepConfig lastDecadeConfig = new StepConfig()
            .withName(lastDecadeClass)
            .withHadoopJarStep(lastDecadeJarConfig)
            .withActionOnFailure(actionOnFailure);

        // Set FMeasure job fow step
        HadoopJarStepConfig fmeasureJarConfig = new HadoopJarStepConfig()
            .withJar(fmeasureJarUrl)
            .withMainClass(fmeasureClass)
            // args[1] is threshold.
            .withArgs(fmeasureInput, fmeasureOutput, args[1]);

        StepConfig fmeasureConfig = new StepConfig()
            .withName(fmeasureClass)
            .withHadoopJarStep(fmeasureJarConfig)
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

        // Set bootstrap action to update lucene version to the one stated in pom.xml.
        BootstrapActionConfig bootstrapConfig = new BootstrapActionConfig()
            .withName("Update Lucene")
            .withScriptBootstrapAction(new ScriptBootstrapActionConfig().withPath(updateLuceneUri));

        // Set job flow request.
        RunJobFlowRequest runFlowRequest = new RunJobFlowRequest()
            .withName(jobName)
            .withAmiVersion(amiVersion)
            .withInstances(instances)
            .withBootstrapActions(bootstrapConfig)
            .withLogUri(logUri)
            // Both parts (A+B), all steps.
            .withSteps(countConfig, joinConfig, calculateConfig, lastDecadeConfig, fmeasureConfig);
            // Custom steps.
            // .withSteps(calculateConfig, lastDecadeConfig);

        // Execute job flow.
        RunJobFlowResult runJobFlowResult = mapReduce.runJobFlow(runFlowRequest);
        String jobFlowId = runJobFlowResult.getJobFlowId();
        System.out.println("ID: " + jobFlowId);
    }
}
