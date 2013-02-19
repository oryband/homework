#ifndef CONNECTION_HANDLER_H_
#define CONNECTION_HANDLER_H_


#include <string>

#include <boost/asio.hpp>


class ConnectionHandler {
    private:
        const std::string _host;
        const short _port;

        boost::asio::io_service _io;
        boost::asio::ip::tcp::socket _socket;

    public:
        ConnectionHandler(const std::string host, const short port);
        virtual ~ConnectionHandler();

        const bool connect();
        void close();

        /* Read a fixed number of bytes from the server - blocking.
         * Returns false in case the connection is closed before bytesToRead bytes can be read.
         */
        const bool getBytes(char bytes[], const unsigned int size);

        /* Send a fixed number of bytes from the client - blocking.
         * Returns false in case the connection is closed before all the data is sent.
         */
        const bool sendBytes(const char bytes[], const unsigned int size);

        /* Read an ascii line from the server
         * Returns false in case connection closed before a newline can be read.
         */
        const bool getLine(std::string& line);

        /* Send an ascii line from the server
         * Returns false in case connection closed before all the data is sent.
         */
        const bool sendLine(const std::string& line);

        /* Get Ascii data from the server until the delimiter character
         * Returns false in case connection closed before null can be read.
         */
        const bool getFrameAscii(std::string& frame, const char delimter);

        /* Send a message to the remote host.
         * Returns false in case connection is closed before all the data is sent.
         */
        const bool sendFrameAscii(const std::string& frame, const char delimter);
};


#endif

