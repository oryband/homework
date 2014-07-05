package com.dsp.ass3;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.logging.ConsoleHandler;
import java.util.logging.Logger;

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
    public static final String
        delim = " ",  // Data delimeter.
        keyValueDelim = "\t",
        joinStart = "*",  // Join start reducer char.
        biarcDelim = "\t",

        bucket = "ory-dsp-ass3",
        s3Uri = "https://s3.amazonaws.com/" + bucket + "/";

    public static final int argInIndex = 0;  // Use =0 for local testing, =1 for AWS.

    private static final Logger logger = setLogger(Logger.getLogger(Utils.class.getName()));


    // Makes a putObjectRequest to make file public, and sends request.
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


    private static String uploadToS3(AmazonS3 s3, PutObjectRequest req, String path) {
        logger.info("Uploading to S3: " + s3Uri + path);

        if (putObject(s3, req)) {
            return generateS3FileAddress(s3, path);
        } else {
            return null;
        }
    }


    // Appends a S3 http:// and bucket address to file path.
    private static String generateS3FileAddress(AmazonS3 s3, String path) {
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


    private static String uploadStringToS3(AmazonS3 s3, String data, String path) {
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
        PutObjectRequest req = new PutObjectRequest(bucket, path, file);

        return uploadToS3(s3, req, path);
    }


    // Creates an S3 connection with socket timeout = 0 (avoids exceptions).
    public static AmazonS3 createS3(AWSCredentials creds) {
        ClientConfiguration config = new ClientConfiguration();
        config.setSocketTimeout(0);
        return new AmazonS3Client(creds, config);
    }


    public static void uploadToS3(String info, String path) {
        AWSCredentials creds = loadCredentials();

        if (creds == null) {
            logger.severe("Couldn't load credentials.");
            return;
        }

        AmazonS3 s3 = createS3(creds);

        uploadStringToS3(s3, info, path);
    }


    // Use custom string format for logger.
    public static Logger setLogger(Logger logger) {
        ShortFormatter formatter = new ShortFormatter();
        ConsoleHandler handler = new ConsoleHandler();

        logger.setUseParentHandlers(false);
        handler.setFormatter(formatter);
        logger.addHandler(handler);

        return logger;
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
}
