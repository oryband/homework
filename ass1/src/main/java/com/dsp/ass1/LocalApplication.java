package com.dsp.ass1;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;


import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;

public class LocalApplication {
    
    private static final Logger logger = Logger.getLogger(LocalApplication.class.getName());
    
    private static void WriteToFile(String fileName, String info) throws FileNotFoundException{
        PrintWriter out;
        
        try {
            out = new PrintWriter(fileName);
        }catch (FileNotFoundException e){
            logger.severe(e.getMessage());
            return;
        }
        
        out.println(info);
        out.close();    
        logger.info("results saved in results.html");
    }
    
    private static String LinkToHTMLString(String link) throws IOException{
        URL url;
        String line;
        StringBuilder content = new StringBuilder();
        InputStream is;
        BufferedReader br;
        
        try {
            url = new URL(link);
        } catch (MalformedURLException e){ 
            logger.severe(e.getMessage());
            return null;
        }
        
        try{
            is = url.openStream();
        } catch (IOException e){ 
            logger.severe(e.getMessage());
            return null;
        }
        
        br = new BufferedReader(new InputStreamReader(is));
       
        content.append("<!DOCTYPE html>\n");
        content.append("<html>\n");
        content.append("<head>\n");
        content.append("<title>Results</title>\n");
        content.append("</head>\n");
        content.append("<body>\n");
        
        while ( (line = br.readLine()) != null){
            content.append(line);
            content.append("<br>\n");
        }
        
        content.append("</body>\n");
        content.append("</html>\n");
        
        try{
            br.close();
        } catch (IOException e){ 
            logger.severe(e.getMessage());
        }
        
        try{
            is.close();
        } catch (IOException e){ 
            logger.severe(e.getMessage());
        }
        
        return content.toString();
    }
    
    
    private static String readFileAsString(String filePath) throws IOException {
        StringBuffer fileData = new StringBuffer();
        BufferedReader reader;
        char[] buf = new char[1024];
        int numRead=0;
        
        try {
            reader = new BufferedReader(new FileReader(filePath));
        }
        catch (FileNotFoundException e){
            logger.severe(e.getMessage());
            return null;
        }
        
        while((numRead=reader.read(buf)) != -1){
            String readData = String.valueOf(buf, 0, numRead);
            fileData.append(readData);
        }
        
        try {
            reader.close();
        }catch (IOException e){
            logger.severe(e.getMessage());
        }
        
        return fileData.toString();
    }
    
    
    public static void main (String[] args) throws IOException{
        
        logger.info("Starting...");
        
        if(args.length == 0) {
            logger.severe("no input : closing...");
            return;
        }
        
        String info = readFileAsString(args[0]);
        if(info == null) {
            logger.severe("Can't read the input file : closing...");
            return;
        }
         
        AWSCredentials creds = Utils.loadCredentials();
        
        if(creds == null) {
            logger.severe("Can't find  the credentials file : closing...");
            return;
        }
        
        String localUrl = "https://sqs.us-west-2.amazonaws.com/340657073537/local",
                bucket = "dsp-ass1",
                path = "input/",
                uploadLink = "",
                resultContent,
                finishLink = "";
        
        AmazonS3 s3 = new AmazonS3Client(creds);
        AmazonSQS sqsLocal = new AmazonSQSClient(creds);
        
        uploadLink = Utils.uploadFileToS3(s3, bucket, path, "input.txt", info);
        Utils.SendMessageToQueue(sqsLocal,localUrl,"new task\t"+uploadLink);  
        ReceiveMessageRequest req = new ReceiveMessageRequest(localUrl);

        List<Message> msgs;
        Message msg;

        // Process messages.
        msgs = Utils.getMessages(req, sqsLocal);
        while (true){
            // Sleep if no messages arrived, and retry re-fetch new ones afterwards.
            if (msgs == null || msgs.size() == 0) {
                logger.info("no messages, sleeping.");

                try {
                    TimeUnit.SECONDS.sleep(5);
                } catch(InterruptedException e) {
                    logger.severe(e.getMessage());
                    return;
                }

            } else {
                msg = msgs.get(0);
                String body = msg.getBody();
                logger.info("received: " + body);

                String[] parts = msg.getBody().split("\t");
                if ((parts.length > 0) && parts[0].equals("done task")){
                    finishLink = parts[1];
                    break;
                }
                else {
                    logger.info("ignoring : " + body);
                }
            }
            msgs = Utils.getMessages(req, sqsLocal);
        }
      
        resultContent = LinkToHTMLString(finishLink);
        if(resultContent == null) {
            logger.severe("Can't find  the open finish tasks file : closing...");
            return;
        } 
        WriteToFile("results.html",resultContent);
        logger.info("finishing...");
    }
}
