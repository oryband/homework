/**
 * Implements a data page.
 *
 * @author Ory Band
 * @version 1.0
 */
public class Page {
    public String data;
    public Page   next, prev;

    /**
     * @param data String data.
     * @param next The next (towards the last, newest) page in RAM.
     * @param prev The previous (towdrds the first, oldest) page in RAM.
     *
     * @return A new initialized Page object.
     */
    public Page(String data, Page next, Page prev) {
        this.data = new String(data);
        this.next = next;
        this.prev = prev;
    }
}

