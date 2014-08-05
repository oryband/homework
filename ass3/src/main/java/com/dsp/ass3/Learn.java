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

    private static final String attributesOpen = "{ ",
                                attributesClose = "}",
                                attributesDelim = ",",
                                tab = "\t";

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

    // Add the instance only if the list size if smaller then 10
    private static void myAdd(List<Instance> list, Instance instance) {
        if (list.size() < 10) {
            list.add(instance);
        }
    }

    // Print each the list with its info at the beginning.
    private static void printList(List<Instance> list, String info) {

        String[] attributes;
        String pos, attribute, strValue;
        Instance instance;

        System.out.println(info);

        for (int i = 0 ; i < list.size() ; i++) {
            instance = list.get(i);

            // Get the attributes from the instance
            attributes = instance.toString().split(attributesDelim);
            System.out.print(attributesOpen);

            // Look for each attribute and print it
            for (int j = 1 ; j < attributes.length ; j++) {
                attribute = attributes[j];
                pos = attribute.substring(0, attribute.indexOf(Utils.delim));
                strValue = instance.attribute(Integer.valueOf(pos)).toString();
                System.out.print(strValue.substring(strValue.indexOf("'") + 1, strValue.lastIndexOf("'")));
                if (j != attributes.length -1) {
                    System.out.print(tab);
                }
            }
            System.out.println(attributesClose);
        }

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
        Instance instance, lastInstance = data.lastInstance();
        boolean correctClassValue, predictedClassValue;
        List<Instance>
            tp = new ArrayList<Instance>(),
            tn = new ArrayList<Instance>(),
            fp = new ArrayList<Instance>(),
            fn = new ArrayList<Instance>();

        // Get the first instance
        int i = 0;
        instance = data.instance(i);

        while ((tp.size() < 10 || tn.size() < 10 || fp.size() < 10 || fn.size() < 10 ) && ! instance.equals(lastInstance) ) {
            //correct class value.
            correctClassValue = instance.classValue() == 0.0 ? false : true ;

            // Delete correct classifcation and let classifier predict its own classification.
            instance.setClassMissing();
            predictedClassValue = cModel.classifyInstance(instance) == 0.0 ? false : true ;

            // Populate tp/tn/fp/fn instance list.
            // *P
            if (predictedClassValue == true) {
                if (correctClassValue == true) {
                    // TP
                    myAdd(tp, instance);
                } else {
                    // FP
                    myAdd(fp, instance);
                }
            } else {
                // *N
                if (correctClassValue == true) {
                    // FN
                    myAdd(fn, instance);
                } else {
                    // TN
                    myAdd(tn, instance);
                }
            }

            // Get the next instance
            i++;
            instance = data.instance(i);
        }

        // Print tp/tn/fp/fn lists.
        printList(tp,"True positive:");
        printList(fp,"False positive:");
        printList(fn,"False negative:");
        printList(tn,"True negative:");


        // Print result.
        System.out.println(eval.toClassDetailsString());
        System.out.println();
        System.out.println(eval.toSummaryString());
    }
}
