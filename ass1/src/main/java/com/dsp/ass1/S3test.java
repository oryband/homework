package com.dsp.ass1;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.CannedAccessControlList;
import com.amazonaws.services.s3.model.PutObjectRequest;


public class S3test {
    private static File createSampleFile() throws IOException {
        System.out.println("1");
        File file = File.createTempFile("aws-java-sdk-", ".txt");
        file.deleteOnExit();
        System.out.println("2");
        Writer writer = new OutputStreamWriter(new FileOutputStream(file));
        writer.write("abcdefghijklmnopqrstuvwxyz\n");
        writer.write("01234567890112345678901234\n");
        writer.write("!@#$%^&*()-=[]{};':',.<>/?\n");
        writer.write("01234567890112345678901234\n");
        writer.write("abcdefghijklmnopqrstuvwxyz\n");
        writer.close();

        return file;
    }
    public static void main(String[] args) throws Exception {
        // Use Ireland region.
        String bucketName = "dsp-ass1";

        AWSCredentials creds = new PropertiesCredentials(
                Worker.class.getResourceAsStream("/AWSCredentials.properties"));

        AmazonS3 s3 = new AmazonS3Client(creds);
        System.out.println("sdsds");
        PutObjectRequest up = new PutObjectRequest(bucketName, "Temp/3.txt", createSampleFile());
        s3.putObject(up.withCannedAcl(CannedAccessControlList.PublicRead));
    }
}