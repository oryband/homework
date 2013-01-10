/**
 * SPL101 PS #01 Threads02.java
 *
 * Two threads attempting to write into a single
 * Printer
 */

public class Threads02
{
   /**
    * Demonstrating problems with access to a shared resource.
    */
    public static void main(String[] a) {
        Printer p = new Printer();
        Thread t1 = new Thread( new SimpleAsynchronousTask("a", p) );
        Thread t2 = new Thread( new SimpleAsynchronousTask("b", p) );

        t1.start();  // prints some lines of aaaa
        t2.start();  // prints some lines of bbbb
    }
}

class SimpleAsynchronousTask implements Runnable {
    Printer m_p;
    String m_name;

    /**
     * Create an asynchronious task with a given name and a printer object.
     * @param name task name
     * @param p    printer object
     */
    SimpleAsynchronousTask(String name, Printer p) {
        m_p = p;
        m_name = name;
    }

    /**
     * Main lifecycle.
     * use the priner object to print the name 50 times.
     */
    public void run() {
        for (int i = 0; i<50; i++) {
            m_p.printALine(i, m_name);
        }
    }
}

/**
 * Demonstrating an unsynchronized shared resource.
 */
class Printer {
    Printer() {}

    /**
     * Print a numbered line of the string 's' concatenated 40 times.
     * @param i line number
     * @param s the string to concatenate
     */
    public void printALine(int i, String s) {
        System.out.print(i + ") ");
        for (int j = 0; j < 40; j++) {
            System.out.print(s);
        }
        System.out.println();
    }
}

/**
 * 0) aaaaaaaaaaaa0) bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbabba
 * aaaaaaaaaaaaaaaaaaaaaaaaaa
 * 1) aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 * 2) aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 * 3) aaaaaaaaaaaaaaaaaaaaaa1) bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
 * aaaaaaaaaaaaaaaaaa
 * 4) aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 * 5) aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 * 6) aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 * 7) aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 * 8) aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 * 9) aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa2) abbba
 * bb10) aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 * 11) aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 */
