#include <boost/thread.hpp>

#include "ConnectionHandler.h"


using std::string;
using std::cout;
using std::cin;
using std::endl;

using boost::thread;
using boost::thread_group;
using boost::system::error_code;
using boost::system::system_error;


void sendLoop(ConnectionHandler* connection, char buf[], const int bufSize) {
    while (true) {
        cin.getline(buf, bufSize);

        string line(buf);

        if ( ! connection->sendLine(line) ) {
            return;
        }

        // Print length of message sent.
        //cout << "--- " << line.length() + 1 << " bytes." << endl;
    }
}


const bool receive(ConnectionHandler& connection, char buf[], string& answer) {
    if ( ! connection.getLine(answer) ) {
        return false;
    }

    // Replace '\n' with C EOS delimeter.
    int len = answer.length();
    answer.resize(len - 1);

    cout << endl << "<<< " << answer << endl;

    // Also show messages length.
    //cout << "<<< " << answer << "(" << len << ")" << endl;

    return true;
}


const bool handleAnswer(string& answer) {
    if (answer == "Goodbye.") {
        cout << "Exiting." << endl << endl;
        return false;
    } else {
        return true;
    }
}


void receiveLoop(ConnectionHandler& connection, char buf[], const int bufSize) {
    while (true) {
        string answer;

        if ( ! receive(connection, buf, answer)
                || ! handleAnswer(answer) ) {
            return;
        }
    }
}


int main(int argc, char* argv[]) {
    if (argc != 2) {
        cout <<
            "Not enough or too many arguments. Use 'client [host]'" << endl;
        exit(1);
    }

    string host = string(argv[1]);
    if (host == "localhost") {
        host = "127.0.0.1";
    }

    ConnectionHandler connection(host, 6667);

    if ( ! connection.connect() ) {
        cout << "Exiting." << endl << endl;
        exit(1);
    } else {
        cout << "Connected." << endl;
    }

    const short bufsize = 1024;
    char buf[bufsize];

    // Read messages from stdin and send to server in another thread.
    thread* inputThread = new thread(sendLoop, &connection, buf, bufsize);
    thread_group threadGroup;
    threadGroup.add_thread(inputThread);

    // Receive messages from server in this thread.
    receiveLoop(connection, buf, bufsize);

    threadGroup.remove_thread(inputThread); // Close & free input thread.
    cout << "Disconnected from server.\nExiting." << endl << endl;
    return 0;
}
