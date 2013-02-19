#include "ConnectionHandler.h"


using std::string;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::exception;

using boost::asio::ip::tcp;
using boost::asio::ip::address;
using boost::asio::buffer;
using boost::system::error_code;
using boost::system::system_error;


ConnectionHandler :: ConnectionHandler(
        const string host, const short port) :
    _host(host), _port(port), _io(), _socket(_io) {}


ConnectionHandler :: ~ConnectionHandler() {
    close();
}


const bool ConnectionHandler :: connect() {
    cout << "Connecting to " << _host << ":" << _port << " ..." << endl;

    try {
        // Server's end-point.
        tcp::endpoint endpoint(address::from_string(_host), _port);
        error_code error;

        _socket.connect(endpoint, error);

        if (error) {
            throw system_error(error);
        }
    } catch (exception& e) {
        cerr << "connect() failed (" << e.what() << ")" << endl;
        return false;
    }

    return true;
}


void ConnectionHandler :: close() {
    try {
        _socket.close();
    } catch (exception& e) {
        cout << "close() failed: connection already closed" << endl;
    }
}

const bool ConnectionHandler :: getBytes(
        char bytes[], const unsigned int size) {

    size_t tmp = 0;
    error_code error;

    try {
        while ( ! error && size > tmp ) {
            tmp += _socket.read_some(
                    buffer(bytes + tmp, size - tmp),
                    error);
        }

        if (error) {
            throw system_error(error);
        }
    } catch (exception& e) {
        //cerr << "getBytes() failed (" << e.what() << ")" << endl;
        return false;
    }

    return true;
}


const bool ConnectionHandler :: sendBytes(
        const char bytes[], const unsigned int size) {
    size_t tmp = 0;  // FIXME should be int?
    error_code error;

    try {
        while ( ! error && size > tmp ) {
            tmp += _socket.write_some(
                    buffer(bytes + tmp, size - tmp),
                    error);
        }
		if (error) {
			throw system_error(error);
        }
    } catch (exception& e) {
        //cerr << "sendBytes() failed (" << e.what() << ")" << endl;
        return false;
    }

    return true;
}


const bool ConnectionHandler :: getLine(string& line) {
    return getFrameAscii(line, '\n');
}


const bool ConnectionHandler :: sendLine(const string& line) {
    return sendFrameAscii(line, '\n');
}


const bool ConnectionHandler :: getFrameAscii(
        string& frame, const char delimeter) {
    char ch;

    do {
        if ( ! getBytes(&ch, 1) ) {
            return false;
        }

        try {
            frame.append(1, ch);
        } catch (exception& e) {
            cerr << "getFrameAscii() failed (" << e.what() << ")" << endl;
            return false;
        }
    } while (delimeter != ch);

    return true;
}


const bool ConnectionHandler :: sendFrameAscii(
        const string& frame, const char delimiter) {

	if ( ! sendBytes(frame.c_str(), frame.length())) {
        return false;
    } else {
        return sendBytes(&delimiter, 1);
    }
}
