/**
 * SPL101 PS #01 Threads03.java
 * 
 * an example to show that under the run of several threads 
 * the trivial pre and post condition are not so trivial
 * we can observe the mistakes in runtime with checking 
 * the pre and post conditions
 * 
 * the problem is when two (or more)
 * threads are running the next() function together
 * we get unexpected increasing and odd numbers are displayed
*/
import java.util.concurrent.*;

public class Threads03 { 
   /**
    * Demonstrating the failing of the pre/post conditions of Even.
    */
    public static void main(String[] args) {
        ExecutorService e = Executors.newFixedThreadPool(10);
        Even ev = new Even();
        for (int i=0; i<10; i++) {
            e.execute(new EvenTask(ev));
        }
        e.shutdown();
    }
}

class EvenTask implements Runnable {
    Even m_even;

    /**
     * Create an EvenTask with a given Even object.
     * @param even an even object
     */
    EvenTask(Even even) {
        m_even = even;
    }

    /**
     * Main lifecycle: print 50 even numbers using the even object.
     */
    public void run() {
        for (int i = 0; i < 50; i++) {
            System.out.println(m_even.next());
        }
    }
}


/**
 * (unsafe) Even object, producing a greater even number on each call to next()
 */
class Even {
    private long n = 0;

    /**
     * return an even number.
     * @return an even number not returned before by this object.
     */
    //@ pre-condition: n is even
    public long next() {
        n++;
        // NOTE: we are going to sleep to make the problem obvious. 
        // In a real application (without the sleep) the problem will be 
        // much rarer (and thats precisely what makes it so dangerous!)
        try {Thread.sleep(30);} catch (InterruptedException e) {}
        n++;
        return n;
    }
    //@ post-condition : n is greater by two

}

/**
 * 11
 * 18
 * 14
 * 16
 * 21
 * 12
 * 19
 * 17
 * 15
 * 13
 * 31
 * 32
 * 33
 */
