package com.dsp.ass1;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.AmazonEC2Client;
import com.amazonaws.services.ec2.model.Instance;
import com.amazonaws.services.ec2.model.InstanceType;
import com.amazonaws.services.ec2.model.RunInstancesRequest;
import com.amazonaws.services.ec2.model.RunInstancesResult;

public class LocalApplication {
    
    private static final Logger logger = Logger.getLogger(LocalApplication.class.getName());
    
    
    public static RunInstancesRequest initRunInstancesRequest(){
        RunInstancesRequest request = new RunInstancesRequest("ami-08728661", 1, 1);
        request.setInstanceType(InstanceType.T1Micro.toString());
        return request;
    }
    
    
    public static void main (String[] args) throws IOException{
        
        logger.info("Starting...");
        
        if(args.length == 0) {
            logger.severe("no input : closing...");
            return;
        }
        
        FileReader file;
        
        try {
            file = new FileReader (args[0]);
        } catch (FileNotFoundException e){
            logger.severe(e.getMessage());
            return;
        }
        
        AWSCredentials credentials;
        
        try {
            credentials = new PropertiesCredentials(LocalApplication.class.getResourceAsStream("/AWSCredentials.properties"));
        } catch (IOException e){
            logger.severe(e.getMessage());
            return;
        }
        
        AmazonEC2 ec2 = new AmazonEC2Client(credentials);
        
        RunInstancesRequest request = initRunInstancesRequest();
        List<Instance> instances = ec2.runInstances(request).getReservation().getInstances();
        System.out.println("Launch instances: " + instances);
        
    }
            
}
