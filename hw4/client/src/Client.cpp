#include "Client.h"


using namespace std;


Client :: Client(string const host, short const port) :
    _connectionHandler(host, port),
    _protocolHandler() {}
