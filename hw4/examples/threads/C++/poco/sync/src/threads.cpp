#include <iostream>
#include "Poco/Runnable.h"
#include "Poco/ThreadPool.h"
#include "Poco/RWLock.h"
 
class Task : public Poco::Runnable{
private:
    int _id;
    Poco::RWLock * _lock;
public:
    Task (int id, Poco::RWLock * lock) : _id(id), _lock(lock) {}
    void run(){
        for (int i= 0; i < 100; i++){
           _lock->writeLock(); //acquire lock
           std::cout << i << ") Task " << _id << " is working" << std::endl; 
          _lock->unlock();  //release lock
        }
    }
};
 
int main(){
    Poco::RWLock lock;
    Task task1(1, &lock);
    Task task2(2, &lock);
    Poco::ThreadPool pool;
    pool.start(task1);
    pool.start(task2);
    pool.joinAll();
    return 0;
}
