package com.dsp.ass2;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.logging.Logger;
import java.util.logging.ConsoleHandler;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.ClientConfiguration;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.CannedAccessControlList;
import com.amazonaws.services.s3.model.PutObjectRequest;


public class Utils {

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Count.class.getName()));

    public static final String delim = "\t",
            bucket = "ory-dsp-ass2",
            filePath = "steps/Records/totalRecord.txt";


    // Use custom string format for logger.
    public static Logger setLogger(Logger logger) {
        ShortFormatter formatter = new ShortFormatter();
        ConsoleHandler handler = new ConsoleHandler();

        logger.setUseParentHandlers(false);
        handler.setFormatter(formatter);
        logger.addHandler(handler);

        return logger;
    }

    // Creates an S3 connection with socket timeout = 0 (avoids exceptions).
    public static AmazonS3 createS3(AWSCredentials creds) {
        ClientConfiguration config = new ClientConfiguration();
        config.setSocketTimeout(0);
        return new AmazonS3Client(creds, config);
    }

    public static PropertiesCredentials loadCredentials() {
        try {
            return new PropertiesCredentials(
                    Utils.class.getResourceAsStream("/AWSCredentials.properties"));
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }
    }

    private static boolean putObject(AmazonS3 s3, PutObjectRequest req) {
        // Set file as public.
        req.withCannedAcl(CannedAccessControlList.PublicRead);

        // Send upload request.
        try {
            s3.putObject(req);
        } catch (AmazonServiceException e) {
            logger.severe(e.getMessage());
            return false;
        } catch (AmazonClientException e) {
            logger.severe(e.getMessage());
            return false;
        }

        return true;
    }


    public static String uploadToS3(AmazonS3 s3, PutObjectRequest req) {
        logger.info("Uploading to S3: " + filePath);

        if (putObject(s3, req)) {
            return generateS3FileAddress(s3, filePath);
        } else {
            return null;
        }
    }

    public static String generateS3FileAddress(AmazonS3 s3, String path) {
        String address;

        try {
            address = s3.getBucketLocation(bucket);
        } catch (AmazonServiceException e) {
            logger.severe(e.getMessage());
            return null;
        } catch (AmazonClientException e) {
            logger.severe(e.getMessage());
            return null;
        }

        return "https://s3-" + address + ".amazonaws.com/" + bucket + "/" + path;
    }



    public static String uploadStringToS3(AmazonS3 s3, String data) {
        // Create temporary file and write to it.
        File file = null;
        try {
            file = File.createTempFile("task", null);
            file.deleteOnExit();

            Writer out = new OutputStreamWriter(new FileOutputStream(file));
            out.write(data);
            out.close();
        } catch (FileNotFoundException e) {
            logger.severe(e.getMessage());
            return null;
        } catch (IOException e) {
            logger.severe(e.getMessage());
            return null;
        }

        // Generate upload request.
        PutObjectRequest req = new PutObjectRequest(bucket, filePath, file);

        return uploadToS3(s3, req);
    }
}
