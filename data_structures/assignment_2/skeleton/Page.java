/**
 * Implements a data page.
 *
 * @author Ory Band
 * @version 1.0
 */
public class Page {
    private int    index;  // Immutable Hard-disk index (set during init).
    private String data;

    public Page next, prev;

    /**
     * @param data  String data.
     * @param index Hard-disk index.
     * @param next  The next (towards the last, newest) page in RAM.
     * @param prev  The previous (towdrds the first, oldest) page in RAM.
     *
     * @return A new initialized Page object.
     */
    public Page(String data, int index, Page next, Page prev) {
        this.index = index;

        this.setData(data);

        this.next = next;
        this.prev = prev;
    }


    /** @return Hard-disk index. */
    public int getIndex() {
        return this.index;
    }


    /** @return Copy of data. */
    public String getData() {
        return new String(this.data);
    }


    /**
     * Sets (clones) string as data.
     *
     * @param s String to clone.
     */
    public void setData(String s) {
        this.data = new String(s);
    }


    /**
     * Appends a single character to the end of the data.
     *
     * @param c Character to append.
     */
    public void appendChar(char c) {
        this.data += c;
    }
}

