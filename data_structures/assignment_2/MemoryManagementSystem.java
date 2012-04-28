import java.util.Arrays;


/**
 * Implements a memory management system.
 *
 * @author Ory Band
 * @version 1.0
 */
public class MemoryManagementSystem {
    private RAM      ram;
    private String[] hd;
    private boolean  lru;


    /**
     * @param lru Use LRU implementation.
     *
     * @return a new initialized MemoryManagementSystem object.
     */
    public MemoryManagementSystem(boolean lru) {
        this.ram = new RAM(1000, 50);
        this.hd  = new String[1000];
        this.lru = lru;

        for (int i=0; i<1000; i++) {
            this.hd[i] = "";
        }
    }


    /** @see java.lang.Object#toString() */
    public String toString() {
        return "secondaryMemory=" + Arrays.toString(this.hd);
    }


    /**
     * Updates Page's data to hard-disk.
     *
     * @param p Page whose data is to be flushed.
     */
    private void flush(Page p) {
        this.hd[p.getIndex()] = p.getData();
    }


    /**
     * Reads Page from RAM, and updates old head page in hard-disk, if thrown out of RAM.
     *
     * @param key Page key in hard-disk.
     *
     * @return Page's data.
     */
    public String read(int key) {
        Page oldHead = this.ram.load(key, this.hd, this.lru);  // Load to RAM if necessary.

        if (oldHead != null) {  // Flush if a new Page was loaded to RAM.
            this.flush(oldHead);
        }

        return this.ram.getPage(key).getData();
    }


    /**
     * Appends a character to the Page in RAM.
     *
     * @param key Page's key in hard-disk.
     * @param c Character to append to Page.
     */
    public void write(int key, char c) {
        Page oldHead = this.ram.load(key, this.hd, this.lru);  // Load to RAM if necessary.

        if (oldHead != null) {  // Flush if a new Page was loaded to RAM.
            this.flush(oldHead);
        }

        this.ram.getPage(key).appendChar(c);
    }
}

