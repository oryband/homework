import java.io.*; import java.net.*;
 
public class Http {
 
        public static void main(String[] args) throws Exception {
                String host = args[0];
                Socket lp = new Socket(host, 80);
                PrintWriter out = new PrintWriter(new OutputStreamWriter(lp.getOutputStream(), "UTF-8"), true);
                out.print ("GET / HTTP/1.0\r\n" +
                           "Host: " + host+ "\r\n" +
                           "\r\n\r\n");
                out.flush();
                BufferedReader in = new BufferedReader(new InputStreamReader(lp.getInputStream(),"UTF-8"));
                String msg = in.readLine();
                while (msg != null) {
                        System.out.println(msg);
                        msg = in.readLine();
                }
 
                out.close();
                in.close();
                lp.close();
        }
 
}

