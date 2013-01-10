/**
 * SPL101 PS #01 Threads01.java
 * 
 * Creating a thread by implementing a Runnable and wrapping it.
 */

public class Threads01 {
    /**
     * Demonstrating thread creation by
     * implementing Runnable and wrapping with a Thread
     */
    public static void main(String[] a) {
        SimpleRunnable r1 = new SimpleRunnable("r1");
        Thread t1 = new Thread(r1);

        SimpleRunnable r2 = new SimpleRunnable("r2");
        Thread t2 = new Thread(r2);

        t1.start();
        t2.start();
    }
}

/**
 * Example for implementing Runnable
 */
class SimpleRunnable implements Runnable {
    private String name;

    SimpleRunnable(String name) {
       this.name = name;
    }

    /**
     * Main lifecycle
     */
    public void run() {
        for (int i = 0; i < 50; i++) {
            System.out.println("RUNNABLE:" + this.name);
            try {
                Thread.sleep(100);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}

/**
 * RUNNABLE:r2
 * RUNNABLE:r1
 * RUNNABLE:r2
 * RUNNABLE:r1
 * RUNNABLE:r1
 * RUNNABLE:r2
 * RUNNABLE:r2
 * RUNNABLE:r1
 * RUNNABLE:r1
 * RUNNABLE:r2
 * RUNNABLE:r2
 */

