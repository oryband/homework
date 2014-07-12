package com.dsp.ass3;

import java.util.Random;

import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.classifiers.bayes.NaiveBayes;
import weka.core.Instances;
import weka.core.converters.ConverterUtils.DataSource;


 public class Learn {
     public static void main(String[] args) throws Exception {
         DataSource source = new DataSource("in/test/data.arff");
         Instances data = source.getDataSet();
         data.setClassIndex(0);

         int folds = 10, seed = 10;

         Random rand = new Random(seed);   // create seeded number generator
         Instances randData = new Instances(data);
         randData.randomize(rand);

         Evaluation eval = new Evaluation(randData);

         for (int n = 0; n < folds; n++) {
             Instances train = randData.trainCV(folds, n);
             Instances test = randData.testCV(folds, n);

             // build and evaluate classifier
             Classifier cModel = (Classifier) new NaiveBayes();
             cModel.buildClassifier(train);
             eval.evaluateModel(cModel, test);
           }

         // Print the result
         String strSummary = eval.toSummaryString();
         System.out.println(strSummary);
     }
 }
