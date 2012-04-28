/**
 * Implements a linked-list array, to be used as runtime-efficient RAM:
 * The array is practically the hard-disk's mirror, so fetching will take O(1).
 * This class also implements a linked-list in some elements of the array,
 * that represent the RAM. This way we can track the virtual RAM queue.
 *
 * @author Ory Band
 * @Version 1.0
 */
public class RAM {
    private Page[] pages;     // Hard-disk mirror.
    private Page head, tail;  // Tracks both ends of the virtual RAM queue.

    /**
     * @param hdSize Hard-disk size.
     * @param ramSize RAM size.
     *
     * @return a new initialized RAM object.
     */
    public RAM(int hdSize, int ramSize) {
        if (ramSize <= 1) {
            throw new RuntimeException("RAM size too small.");
        } else if (hdSize <= 1) {
            throw new RuntimeException("HD size too small.");
        } else if (hdSize < ramSize) {
            throw new RuntimeException("HD size smaller than RAM size.");
        }

        this.pages = new Page[hdSize];

        // Init data.
        for (int i=0; i<hdSize; i++) {
            this.pages[i] = new Page("", i, null, null);
        }

        for (int i=0; i<ramSize -1; i++) {
            // Set backwards order.
            if (i >= 1) {
                this.pages[i].prev = this.pages[i-1];
            }

            // Set advancing order.
            if (i <= ramSize -2) {
                this.pages[i].next = this.pages[i+1];
            }
        }

        // Set head and tail queue Pages.
        this.head = this.pages[0];
        this.tail = this.pages[ramSize -1];
    }


    /**
     * Loads page into RAM, and returns old head Page.
     * Distinguishes between FIFO/LRU.
     *
     * @param key Page's key in hard-disk.
     * @param hd Hard-disk String array.
     * @param lru FIFO/LRU switch.
     * 
     * @return Old head Page, if a new Page was loaded onto RAM.
     */
    public Page load(int key, String[] hd, boolean lru) {
        Page p = this.pages[key];

        // Load data to RAM queue if not present, and return old head Page.
        if (p.prev == null && p.next == null) {
            p.setData(hd[key]);  // Load data from hard-disk.
            this.enqueue(p);
            return this.dequeue();
        // If data already in RAM and LRU is on,
        // Relocate data to RAM's queue end.
        } else if (lru) {
            if (p.prev == null) {  // This is the head Page.
                this.dequeue();  // Like `remove()`, but sets new head Page.
                this.enqueue(p);
            } else if (p.next != null) {  // This is NOT the tail Page.
                this.remove(p);
                this.enqueue(p);
            }
        }

        return null;
    }


    /**
     * @param key Page's key in hard-disk.
     *
     * @return Page by key given as argument.
     */
    public Page getPage(int key) {
        return this.pages[key];
    }


    /**
     * Removes (detahces) Page received as argument from RAM queue.
     *
     * @param key Page key in hard-disk.
     */
    public void remove(Page p) {
        if (p.prev != null) {
            p.prev.next = p.next;
        }

        if (p.next != null) {
            p.next.prev = p.prev;
        }

        p.next = null;
        p.prev = null;
    }


    /**
     * Dequeues and returns the RAM's head page.
     *
     * @return RAM's head page. */
    public Page dequeue() {
        Page oldHead = this.head;
        this.head = this.head.next;
        this.remove(oldHead);
        return oldHead;
    }


    /**
     * Adds page to end of queue, if not already present.
     *
     * @param key Page key in hard-disk.
     */
    public void enqueue(Page p) {
        if (p.prev != null || p.next != null) {  // Poll if Page is already in RAM.
            throw new RuntimeException("Page already in RAM.");
        } else {
            // Enqueue as tail Page.
            p.prev = this.tail;
            p.next = null;

            // Set as tail in queue.
            this.tail.next = p;
            this.tail = p;
        }
    }
}

