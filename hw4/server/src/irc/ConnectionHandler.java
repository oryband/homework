/** @author Eldar Damari, Ory Band. */

package irc;

import java.nio.ByteBuffer;


public interface ConnectionHandler<T> {
	public void addOutData(ByteBuffer buf);
	public void addOutData(T msg);
	public void read();
	public void switchToReadOnlyMode();
	public void switchToWriteOnlyMode();
	public void switchToReadWriteMode();
	public void write();
}
