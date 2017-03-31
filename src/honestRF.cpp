#include "honestRF.h"
#include <random>
#include <algorithm>
#include <thread>
#include <mutex>

honestRF::honestRF():
  _trainingData(0), _ntree(0), _replace(0), _sampSize(0), _splitRatio(0),
  _mtry(0), _nodeSizeSpt(0), _nodeSizeAvg(0), _forest(0), _seed(0),
  _verbose(0){};

honestRF::~honestRF(){};

honestRF::honestRF(
  DataFrame* trainingData,
  size_t ntree,
  bool replace,
  size_t sampSize,
  double splitRatio,
  size_t mtry,
  size_t nodeSizeSpt,
  size_t nodeSizeAvg,
  unsigned int seed,
  bool verbose
){
  this->_trainingData = trainingData;
  this->_ntree = ntree;
  this->_replace = replace;
  this->_sampSize = sampSize;
  this->_splitRatio = splitRatio;
  this->_mtry = mtry;
  this->_nodeSizeSpt = nodeSizeSpt;
  this->_nodeSizeAvg = nodeSizeAvg;
  this->_seed = seed;
  this->_verbose = verbose;

  if (splitRatio > 1 || splitRatio < 0) {
    throw "splitRatio shoule be between 0 and 1.";
  }

  std::vector<honestRFTree> *forest = new std::vector<honestRFTree>;

  const size_t maxThreads = std::thread::hardware_concurrency();
//  std::cout << "Parallel using " << maxThreads << " threads..." << std::endl;
  std::vector<std::thread> allThreads(maxThreads);
  std::mutex threadLock;

  for (size_t t = 0; t < maxThreads; t++) {
    auto dummyThread = std::bind(
      [&](const int iStart, const int iEnd, const int t) {
        // loop over all items
        for (int i = iStart; i < iEnd; i++){

          unsigned int myseed = getSeed() * (i + 1);

          std::vector<size_t> sampleIndex;
          if (isReplacement()) {
            while (sampleIndex.size() < getSampleSize()) {
              size_t randomIndex = (size_t)
                      (rand_r(&myseed) % ((int) (*getTrainingData()).getNumRows()));
              sampleIndex.push_back(randomIndex);
            }
          } else {
            while (sampleIndex.size() < getSampleSize()) {
              size_t randomIndex = (size_t)
                      (rand_r(&myseed) % ((int) (*getTrainingData()).getNumRows()));
              if (sampleIndex.size() == 0 ||
                  std::find(sampleIndex.begin(),
                            sampleIndex.end(),
                            randomIndex) == sampleIndex.end()) {
                sampleIndex.push_back(randomIndex);
              }
            }
          }

          std::vector<size_t> *splitSampleIndex;
          std::vector<size_t> *averageSampleIndex;

          if (splitRatio == 1 || splitRatio == 0) {
            // Treat it as normal RF
            splitSampleIndex = new std::vector<size_t>(sampleIndex);
            averageSampleIndex = new std::vector<size_t>(sampleIndex);
          } else {
            size_t splitSampleSize = (size_t) (getSplitRatio() * sampSize);
            size_t averageSampleSize = sampSize - splitSampleSize;
            if (splitSampleSize < 2 * nodeSizeSpt ||
                averageSampleSize < 2 * nodeSizeAvg) {
              throw "splitRatio too big or too small.";
            }

            // Generate sample index
            splitSampleIndex = new std::vector<size_t>;
            averageSampleIndex = new std::vector<size_t>;

            for(std::vector<size_t>::iterator it=sampleIndex.begin();
                it!=sampleIndex.end(); ++it) {
              if ((*splitSampleIndex).size() < splitSampleSize) {
                (*splitSampleIndex).push_back(*it);
              } else {
                (*averageSampleIndex).push_back(*it);
              }
            }
          }

          honestRFTree *oneTree = new honestRFTree(
            getTrainingData(), getMtry(),
            getNodeSizeSpt(), getNodeSizeAvg(),
            splitSampleIndex, averageSampleIndex,
            myseed
          );

          std::lock_guard<std::mutex> lock(threadLock);
          if (isVerbose()) {
            std::cout << "Finish training tree # " << (i + 1) << std::endl;
          }
          (*forest).push_back(*oneTree);
        }
      },
      t * getNtree() / maxThreads,
      (t + 1) == maxThreads ? getNtree() : (t + 1) * getNtree() / maxThreads,
      t
    );
    allThreads[t] = std::thread(dummyThread);
  }

  std::for_each(
    allThreads.begin(),
    allThreads.end(),
    [](std::thread& x){x.join();}
  );

  this->_forest = forest;
}

std::vector<double>* honestRF::predict(
  std::vector< std::vector<double> >* xNew
){
  std::vector<double> *prediction = new std::vector<double>;
  size_t numObservations = (*xNew)[0].size();
  for (size_t j=0; j<numObservations; j++){
    (*prediction).push_back(0);
  }

  const size_t maxThreads = std::thread::hardware_concurrency();
//  std::cout << "Parallel using " << maxThreads << " threads..." << std::endl;
  std::vector<std::thread> allThreads(maxThreads);
  std::mutex threadLock;

  for (size_t t = 0; t < maxThreads; t++) {
    auto dummyThread = std::bind(
      [&](const int iStart, const int iEnd, const int t) {
        // loop over all items
        for (int i=iStart; i < iEnd; i++){
          std::vector<double> currentTreePrediction(numObservations);
          honestRFTree currentTree = (*getForest())[i];
          currentTree.predict(currentTreePrediction, xNew);
          std::lock_guard<std::mutex> lock(threadLock);
          for (size_t j=0; j<numObservations; j++){
            (*prediction)[j] += currentTreePrediction[j];
          }
        }
      },
      t * getNtree() / maxThreads,
      (t + 1) == maxThreads ? getNtree() : (t + 1) * getNtree() / maxThreads,
      t
    );
    allThreads[t] = std::thread(dummyThread);
  }

  std::for_each(
          allThreads.begin(),
          allThreads.end(),
          [](std::thread& x){x.join();}
  );

  for (size_t j=0; j<numObservations; j++){
    (*prediction)[j] /= getNtree();
  }
  return prediction;
}