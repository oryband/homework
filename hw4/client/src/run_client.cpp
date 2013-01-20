#include "ConnectionHandler.h"


using std::string;
using std::cout;
using std::cin;
using std::endl;

using boost::system::error_code;
using boost::system::system_error;


void send(ConnectionHandler& connection, char buf[], const int bufsize) {
    int len;

    cout << "$ ";
    cin.getline(buf, bufsize);

    string line(buf);
    len = line.length();

    if ( ! connection.sendLine(line) ) {
        cout << "Disconnected from server. Exiting." << endl << endl;

        return;
    }

    //cout << ">>> " << len + 1 << " bytes." << endl;
}


const bool receive(ConnectionHandler& connection, char buf[], string& answer) {
    int len;

    if ( ! connection.getLine(answer) ) {
        cout << "Disconnected from server. Exiting." << endl << endl;

        return false;
    }

    // Replace '\n' with C EOS delimeter.
    len = answer.length();
    answer.resize(len - 1);

    cout << "<<< " << answer << "(" << len << ")" << endl;

    return true;
}


const bool handleAnswer(string& answer) {
    if (answer == "bye") {
        cout << "Exiting." << endl << endl;
        return false;
    } else {
        return true;
    }
}


void ioLoop(ConnectionHandler& connection) {
    const short bufsize = 1024;
    char buf[bufsize];

    while (true) {
        string answer;

        if ( ! receive(connection, buf, answer) ) {
            return;
        }

        send(connection, buf, bufsize);

        if ( ! handleAnswer(answer) ) {
            return;
        }
    }
}


int main(int argc, char* argv[]) {
    if (argc != 2) {
        cout <<
            "Not enough or too many arguments. Use 'run_client [host]'" << endl;
        exit(1);
    } else {
        const string host = string(argv[1]);

        ConnectionHandler connection(host, 6667);

        if ( ! connection.connect() ) {
            cout << "Exiting." << endl << endl;
            exit(1);
        } else {
            cout << "Connected." << endl;
        }

        ioLoop(connection);
    }

    return 0;
}
