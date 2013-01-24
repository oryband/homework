package irc;

import java.nio.channels.SocketChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.ClosedChannelException;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.io.IOException;
import java.net.SocketAddress;
import java.util.Vector;
import java.util.logging.Logger;


/**
 * Handles messages from clients
 */
public class TpcConnectionHandler<String> implements ConnectionHandler<String>, Runnable {

	private static final int BUFFER_SIZE = 1024;

	protected final SocketChannel _sChannel;

	protected final AsyncServerProtocol<String> _protocol;
	protected final MessageTokenizer<String> _tokenizer;

	protected Vector<ByteBuffer> _outData = new Vector<ByteBuffer>();

	private static final Logger logger = Logger.getLogger("edu.spl.reactor");


	/**
	 * Creates a new ConnectionHandler object
	 * 
	 * @param sChannel
	 *            the SocketChannel of the client
	 * @param data
	 *            a reference to a ReactorData object
	 */
	private TpcConnectionHandler(
            SocketChannel sChannel,
            AsyncServerProtocol<String> protocol,
            MessageTokenizer<String> tokenizer) {

		_sChannel  = sChannel;
		_protocol  = protocol;
		_tokenizer = tokenizer;
	}


	public static <String> TpcConnectionHandler<String> create(
            SocketChannel sChannel,
            AsyncServerProtocol<String> protocol,
            MessageTokenizer<String> tokenizer) {

		TpcConnectionHandler<String> h = new TpcConnectionHandler<String>(sChannel, protocol, tokenizer);

		return h;
	}


    public synchronized void run() {
        // go over all complete messages and process them.
        while (_tokenizer.hasMessage()) {
            String msg = _tokenizer.nextMessage();
            String response = this._protocol.processMessage(msg);

            // Reply to client if necessary.
            if (response != null) {
                try {
                    ByteBuffer bytes = _tokenizer.getBytesForMessage(response);
                    addOutData(bytes);
                } catch (CharacterCodingException e) { e.printStackTrace(); }
            }
        }

        closeConnection();
    }


	private void closeConnection() {
		// remove from the selector.
		try {
			_sChannel.close();
		} catch (IOException ignored) {
			ignored = null;
            System.out.println("Error closing connectionHandler.");
		}
	}


	/**
	 * Reads incoming data from the client:
	 * <UL>
	 * <LI>Reads some bytes from the SocketChannel
	 * <LI>create a protocolTask, to process this data, possibly generating an
	 * answer
	 * <LI>Inserts the Task to the ThreadPool
	 * </UL>
	 * 
	 * @throws
	 * 
	 * @throws IOException
	 *             in case of an IOException during reading
	 */
	public void read() {
		// do not read if protocol has terminated. only write of pending data is
		// allowed
		if (_protocol.shouldClose()) {
			return;
		}

		SocketAddress address = _sChannel.socket().getRemoteSocketAddress();
		logger.info("Reading from " + address);

		ByteBuffer buf = ByteBuffer.allocate(BUFFER_SIZE);
		int numBytesRead = 0;
		try {
			numBytesRead = _sChannel.read(buf);
		} catch (IOException e) {
			numBytesRead = -1;
		}
		// is the channel closed??
		if (numBytesRead == -1) {
			// No more bytes can be read from the channel
			logger.info("client on " + address + " has disconnected");
			closeConnection();
			// tell the protocol that the connection terminated.
			_protocol.connectionTerminated();
			return;
		}

		//add the buffer to the protocol task
		buf.flip();
        this._tokenizer.addBytes(buf);
	}


	/**
	 * attempts to send data to the client
	 * 
	 * @throws IOException
	 *             if the write operation fails
	 * @throws ClosedChannelException
	 *             if the channel have been closed while registering to the Selector
	 */
	public synchronized void write() {
        // if nothing left in the output string, return.
		if (_outData.size() == 0) {
			return;
		}

		// if there is something to send, send it.
		ByteBuffer buf = _outData.remove(0);
		if (buf.remaining() != 0) {
			try {
				_sChannel.write(buf);
			} catch (IOException e) {
				// this should never happen.
				e.printStackTrace();
			}
			// check if the buffer contains more data
			if (buf.remaining() != 0) {
				_outData.add(0, buf);
			}
		}

		// check if the protocol indicated close.
		if (_protocol.shouldClose()) {
			if (buf.remaining() == 0) {
				closeConnection();
				SocketAddress address = _sChannel.socket().getRemoteSocketAddress();
				logger.info("disconnecting client on " + address);
			}
		}
	}


    /**
     * @param buf byte string to be added to message queue.
     */
	public synchronized void addOutData(ByteBuffer buf) {
		_outData.add(buf);
	}
}
