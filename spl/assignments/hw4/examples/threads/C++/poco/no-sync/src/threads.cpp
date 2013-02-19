#include <iostream>
#include "Poco/Runnable.h"
#include "Poco/ThreadPool.h"
 
class Task : public Poco::Runnable {
private:
    int _id;
public:
    Task (int number) : _id(number) {}
    void run(){
        for (int i= 0; i < 100; i++){
           std::cout << i << ") Task " << _id << " is working" << std::endl; 
        }
    }
};
 
int main(){
    Task task1(1);
    Task task2(2);
    Poco::ThreadPool pool;
    pool.start(task1);
    pool.start(task2);
    pool.joinAll();
    return 0;
}
