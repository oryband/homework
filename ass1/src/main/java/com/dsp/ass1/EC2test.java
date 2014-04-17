package com.dsp.ass1;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.AmazonEC2Client;
import com.amazonaws.services.ec2.model.RunInstancesRequest;

public class EC2test {
       
    public static void main(String[] args) throws Exception {
        AmazonEC2 ec2;
        
        AWSCredentials credentials = Utils.loadCredentials();
        ec2 = new AmazonEC2Client(credentials);
        try {
            // Basic 32-bit Amazon Linux AMI 1.0 (AMI Id: ami-51792c38)
            RunInstancesRequest request = new RunInstancesRequest();
            request.withImageId("ami-51792c38")
            .withInstanceType("t1.micro")
            .withMinCount(1)
            .withMaxCount(1)
            .withKeyName("liran-ec2");
            
            
            ec2.runInstances(request);
          
 
        } catch (AmazonServiceException ase) {
            System.out.println("Caught Exception: " + ase.getMessage());
            System.out.println("Reponse Status Code: " + ase.getStatusCode());
            System.out.println("Error Code: " + ase.getErrorCode());
            System.out.println("Request ID: " + ase.getRequestId());
        }
        
        System.out.println("finished");
 
    }

}
