/**
 * Implements a data page.
 *
 * @author Ory Band
 * @version 1.0
 */
public class Page {
    private int   index;  // Immutable Hard-disk index (set during init).

    public String data;
    public Page   next, prev;

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

        this.data  = new String(data);
        this.next  = next;
        this.prev  = prev;
    }


    /** @return Hard-disk index. */
    public getIndex() {
        return this.index;
    }
}

