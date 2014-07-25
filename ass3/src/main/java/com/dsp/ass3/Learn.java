package com.dsp.ass3;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.classifiers.bayes.NaiveBayes;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.ConverterUtils.DataSource;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.s3.AmazonS3;


public class Learn {
    private static int classIndex = 0;

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Learn.class.getName()));

    // Read source file according to protocol ('s3' or local-file) and return InputStream.
    private static InputStream readSource(String protocol, String path) {
        InputStream is;

        // Read path from S3 and return input stream.
        if (protocol.toLowerCase().equals("s3")) {
            // Init S3 connection.
            AWSCredentials creds = Utils.loadCredentials();
            if (creds == null) {
                logger.severe("Couldn't load credentials.");
                return null;
            }

            AmazonS3 s3 = Utils.createS3(creds);

            // Download file as stream.
            logger.info("Downloading stream from S3.");

            is = Utils.readFromS3(s3, path);
            if (is == null) {
                logger.severe("Error reading input file.");
                return null;
            }

            return is;
        } else {
            // Else read and return local file as stream.
            File file = new File(path);
            try {
                is = new FileInputStream(file);
            } catch (Exception e) {
                logger.severe("Error opening local file.");
                return null;
            }

            return is;
        }
    }


    // Parse InputStream data and return WEKA instances, ready for learning.
    private static Instances readData(String protocol, String path) {
        logger.info("Reading file: " + path);

        InputStream is = readSource(protocol, path);
        if (is == null) {
            return null;
        }

        DataSource source = new DataSource(is);

        // Parse source.
        logger.info("Parsing data.");

        Instances data;
        try {
            data = source.getDataSet(classIndex);
        } catch (Exception e) {
            logger.severe(e.getMessage());
            return null;
        }

        if (data == null) {
            logger.severe("Error parsing source.");
            return null;
        }

        return data;
    }


    public static void main(String[] args) throws Exception {
        // Read path.
        logger.info("Reading path: " + args[1]);

        // Parse data.
        Instances data = readData(args[0], args[1]);
        if (data == null) {
            return;
        }

        logger.info("Finished parsing data.");

        // Randomize seed.
        logger.info("Randomizing instances.");

        int seed = 10;
        Instances randData = new Instances(data);
        randData.randomize(new Random(seed));

        // Read randomized data.
        Evaluation eval = new Evaluation(randData);

        // Learn and perform 10-fold validation.
        Classifier cModel = null;
        int folds = 10;
        for (int n=0; n < folds; n++) {
            logger.info("Fold #" + (n+1));

            // Set train/test instances.
            Instances train = randData.trainCV(folds, n);

            Instances test = randData.testCV(folds, n);

            // Build and evaluate classifier using NaiveBayes.
            cModel = (Classifier) new NaiveBayes();

            logger.info("\tTraining.");
            cModel.buildClassifier(train);

            logger.info("\tEvaluating.");
            eval.evaluateModel(cModel, test);
        }

        // Use the last classifier (from the 10th fold) to classify,
        // and fetch 10 pairs from each TP/TN/FP/FN instance.
        Instance instance;
        boolean correctClassValue, predictedClassValue;
        List<Instance>
            tp = new ArrayList<Instance>(),
            tn = new ArrayList<Instance>(),
            fp = new ArrayList<Instance>(),
            fn = new ArrayList<Instance>();

        for (int i=0; i < 100; i++) {
            // Get instance and correct class value.
            instance = data.instance(i);
            correctClassValue = instance.classValue() == 0.0 ? false : true ;

            // Delete correct classifcation and let classifier predict its own classification.
            instance.setClassMissing();
            predictedClassValue = cModel.classifyInstance(instance) == 0.0 ? false : true ;

            // Populate tp/tn/fp/fn instance list.
            // *P
            if (predictedClassValue == true) {
                // TP
                if (correctClassValue == true) {
                    tp.add(instance);
                } else {
                    fp.add(instance);
                }
            } else {
                // *N
                if (correctClassValue == true) {
                    // FN
                    fn.add(instance);
                } else {
                    // TN
                    tn.add(instance);
                }
            }
        }

        // TODO Print at most 10 instances (vectors) of each type to seperate files.
        int c = 0;
        for (Instance ins : tp) {
            // Print at most 10 instances.
            if (c == 10) {
                break;
            }

            c++;
        }


        // Print result.
        System.out.println(eval.toClassDetailsString());
        System.out.println();
        System.out.println(eval.toSummaryString());
    }
}
