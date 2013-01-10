#ifndef CONNECTION_HANDLER_H_
#define CONNECTION_HANDLER_H_


#include <string>


class ConnectionHandler {
    private:
        const std::string _host;
        const short _port;

    public:
        ConnectionHandler(const std::string host, const short port);
};


#endif

