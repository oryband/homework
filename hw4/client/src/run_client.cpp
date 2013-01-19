#include "ConnectionHandler.h"
//#include "ProtocolHandler.h"
//#include "Encoder.h"


using std::string;
using std::cout;
using std::cin;
using std::endl;

using boost::system::error_code;
using boost::system::system_error;


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

        //Encoder encoder();

        const short bufsize = 1024;
        char buf[bufsize];
        string answer;  // Server answer.
        int len;  // Length of line sent / received.

        while (true) {
            cin.getline(buf, bufsize);

            string line(buf);
            len = line.length();

            if ( ! connection.sendLine(line) ) {
                cout << "Disconnected from server. Exiting." << endl << endl;

                break;
            }

            cout << ">>> " << len + 1 << " bytes" << endl;

            if ( ! connection.getLine(answer) ) {
                cout << "Disconnected from server. Exiting." << endl << endl;

                break;
            }

            len = answer.length();
            answer.resize(len - 1);  // Replace '\n' with C EOS delimeter.
            cout << "<<< " << answer << "(" << len << ")" << endl;

            if (answer == "bye") {
                cout << "Exiting." << endl << endl;

                break;
            }
        }
    }

    return 0;
}
