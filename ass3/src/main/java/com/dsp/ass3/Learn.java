package com.dsp.ass3;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Random;
import java.util.logging.Logger;

import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.classifiers.bayes.NaiveBayes;
import weka.core.Instances;
import weka.core.converters.ConverterUtils.DataSource;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.s3.AmazonS3;


public class Learn {
    private static int classIndex = 0;

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Learn.class.getName()));

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

        // Randomize seed.
        int seed = 10;
        Instances randData = new Instances(data);
        randData.randomize(new Random(seed));

        // Read randomized data.
        Evaluation eval = new Evaluation(randData);

        // Learn and perform 10-fold validation.
        int folds = 10;
        for (int n=0; n < folds; n++) {
            // Set train/test instances.
            Instances train = randData.trainCV(folds, n),
                      test = randData.testCV(folds, n);

            // Build and evaluate classifier.
            Classifier cModel = (Classifier) new NaiveBayes();
            cModel.buildClassifier(train);
            eval.evaluateModel(cModel, test);
        }

        // Print result.
        System.out.println(eval.toClassDetailsString());
        System.out.println(eval.toSummaryString());
    }
}
