#ifndef CLIENT_H_
#define CLIENT_H_


#include <string>

#include "ConnectionHandler.h"
#include "ProtocolHandler.h"


class Client {
    private:
        const ConnectionHandler _connectionHandler;
        const ProtocolHandler _protocolHandler;

    public:
        Client(const std::string host, const short port);
};


#endif
