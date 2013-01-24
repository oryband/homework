/** @author Eldar Damari, Ory Band. */

package irc;

import java.nio.ByteBuffer;


public interface ConnectionHandler<T> {
	public void addOutData(ByteBuffer buf);
	public void read();
	public void write();
}
