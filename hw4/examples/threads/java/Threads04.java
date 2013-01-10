/**
 * SPL101 PS #01 Threads04.java
 * 
 * checking the pre & post conditions, throwing an exception if they are not met 
 */
import java.util.concurrent.*;

public class Threads04{
   /**
    * Demonstrating the failing of the pre/post conditions of Even,
    * but throwing an exception on failure.
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
        try {
            for (int i = 0; i < 50; i++) {
                System.out.println(m_even.next());
            }
        } catch (NotEvenException e) {
            System.out.println("Exception in " + Thread.currentThread().getName());
            e.printStackTrace(System.out);
        }
    }
}


// Note: this solution will probably work 99.999% of the time, but it is still as 
// broken as the last solution. Can you see why?
class Even {
    private long n = 0;

    /**
     * return an even number.
     * @return an even number not returned before by this object.
     * @throws NotEvenException in case a pre- or post- condition is broken.
     */
    //@ pre-condition: n is even
    public long next() throws NotEvenException {
        if (n%2 != 0) {
            throw new NotEvenException("PRE: n is not even!");
        }
        n++;
        try {Thread.sleep(30);} catch (InterruptedException e) {}
        n++;
        if (n%2 != 0) {
            throw new NotEvenException("POST: n is not even!");
        }
        return n;
    }
    //@ post-condition : n is greater in two
}

class NotEvenException extends Exception {
   /**
    * Create an exception with the given message.
    * @param message a message for the exception.
    */
    public NotEvenException (String message) {super(message);}
}

/**
 * Exception in Thread-2
 * NotEvenException: PRE: n is not even!
 *         at Even.next(Threads08.java:43)
 *         at EvenThread.run(Threads08.java:28)
 * Exception in Thread-6
 * NotEvenException: PRE: n is not even!
 *         at Even.next(Threads08.java:43)
 *         at EvenThread.run(Threads08.java:28)
 * Exception in Thread-9
 * NotEvenException: PRE: n is not even!
 *         at Even.next(Threads08.java:43)
 *         at EvenThread.run(Threads08.java:28)
 * Exception in Thread-10
 * NotEvenException: PRE: n is not even!
 *         at Even.next(Threads08.java:43)
 *         at EvenThread.run(Threads08.java:28)
 * 2
 * 4
 * 6
 * 8
 */
