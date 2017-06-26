#include <Rcpp.h>
using namespace Rcpp;
#include <random>
#include <iostream>
#include <vector>
#include <string>
#include <thread>
#include <mutex>

// This files is only called for the unit testing and it tests that the
// randomization is working as it is supposed to work.
//


// [[Rcpp::export]]
double test_rnd() {
  unsigned int myseed = 100;
  return rand_r(&myseed) % 10000;
}


// [[Rcpp::export]]
NumericVector test_rnd2() {
  int newStartingTreeNumber = 1;
  int ntree= 4;
  int newEndingTreeNumber = newStartingTreeNumber + ntree;
  NumericVector b(40, 2.0);

  size_t nthreadToUse = 2;
  //std::cout << "Training parallel using " << nthreadToUse << " threads"
  //          << std::endl;

  std::vector<std::thread> allThreads(nthreadToUse);
  std::mutex threadLock;

  // For each thread, assign a sequence of tree numbers that the thread
  // is responsible for handling
  for (size_t t = 0; t < nthreadToUse; t++) {
    auto dummyThread = std::bind(
      [&](const int iStart, const int iEnd, const int t_) {

        // loop over al assigned trees, iStart is the starting tree number
        // and iEnd is the ending tree number
        for (int i = iStart; i < iEnd; i++) {

          unsigned int myseed = 100 * (i + 1);

          std::mt19937 mt_rand(myseed);


          std::lock_guard<std::mutex> lock(threadLock);

          for (int j=0; j<10; j++) {
            b[j + (i - 1) * 10] = mt_rand() % 10000;
            //std::cout << "Thread " << i << ": " << b[j + (i - 1) * 10] << std::endl;
          }

        }

      },
      newStartingTreeNumber + t * ntree / nthreadToUse,
      (t + 1) == nthreadToUse ?
      (size_t) newEndingTreeNumber :
        newStartingTreeNumber + (t + 1) * ntree / nthreadToUse,
        t
    );
    allThreads[t] = std::thread(dummyThread);
  }

  std::for_each(
    allThreads.begin(),
    allThreads.end(),
    [](std::thread& x){x.join();}
  );
  return b;
}

/*** R
test_rnd()
test_rnd2()
*/





