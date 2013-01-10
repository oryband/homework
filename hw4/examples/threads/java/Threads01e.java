/**
 * SPL101 PS #01 Threads01e.java
 * 
 * Running threads by use of an Executor.
 *
 * The executor interface is:
 *
 * public interface Executor {
 *    public void execute(Runnable r);
 * }
 *
 * And it is used to decouple task creation from task execution -- we don't
 * care how and when a task is executed (as long as it happens).
 *
 * We will be using the ServiceExecutor interface, which extends Executor to provide
 * ways of managing termination (and provides ways for tracking progress of the asynchronous 
 * tasks, but we will not get into it in this tutorial session).
 */
import java.util.concurrent.*;

public class Threads01e {
    /**
     * Demonstrating the creation and use of an Executor to 
     * run several tasks (Threads) implmenting the Runnable interface.
     */
    public static void main(String[] a) {
        // Create an executor:
        ExecutorService e = Executors.newFixedThreadPool(3);

        // create several runnables, and execute them.
        for(int i=0;i<10;i++) {
           System.out.println("creating a new task: " + i + " ");
           SimpleRunnable r = new SimpleRunnable(i);
           e.execute(r);
        }
        e.shutdown(); // this causes the executor not to accept any more tasks, and to 
                      // kill all of its threads when all the submitted tasks are done.
    }
}

/**
 * An example of a simple task implementing the Runnable interface.
 */
class SimpleRunnable implements Runnable {
   private int m_number;

   /**
    * Create a task with a given ID
    * @param i ID of the task
    */
   public SimpleRunnable(int i) {
      this.m_number = i;
   }

    /**
     * Main lifecycle of the task. 
     * Prints the task ID 10 times.
     */
    public void run() {
        for (int i = 0; i < 10; i++) {
            System.out.print(" " + m_number); 
        }
    }
}


