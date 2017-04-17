#include "honestRF.h"
#include <random>
#include <thread>
#include <mutex>
#define DOPARELLEL true

honestRF::honestRF():
  _trainingData(nullptr), _ntree(0), _replace(0), _sampSize(0),
  _splitRatio(0), _mtry(0), _nodeSizeSpt(0), _nodeSizeAvg(0),
  _forest(nullptr), _seed(0), _verbose(0), _nthread(0), _OOBError(0){};

honestRF::~honestRF(){
//  for (std::vector<honestRFTree*>::iterator it = (*_forest).begin();
//       it != (*_forest).end();
//       ++it) {
//    delete(*it);
//  }
//  std::cout << "honestRF() destructor is called." << std::endl;
};

honestRF::honestRF(
  std::unique_ptr<DataFrame> trainingData,
  size_t ntree,
  bool replace,
  size_t sampSize,
  double splitRatio,
  size_t mtry,
  size_t nodeSizeSpt,
  size_t nodeSizeAvg,
  unsigned int seed,
  size_t nthread,
  bool verbose
){
  this->_trainingData = std::move(trainingData);
  this->_ntree = ntree;
  this->_replace = replace;
  this->_sampSize = sampSize;
  this->_splitRatio = splitRatio;
  this->_mtry = mtry;
  this->_nodeSizeSpt = nodeSizeSpt;
  this->_nodeSizeAvg = nodeSizeAvg;
  this->_seed = seed;
  this->_nthread = nthread;
  this->_verbose = verbose;

  if (splitRatio > 1 || splitRatio < 0) {
    throw std::runtime_error("splitRatio shoule be between 0 and 1.");
  }

  std::unique_ptr< std::vector< std::unique_ptr< honestRFTree > > > forest (
    new std::vector< std::unique_ptr< honestRFTree > >
  );

  size_t nthreadToUse = nthread;
  if (nthread == 0) {
    // Use all threads
    nthreadToUse = std::thread::hardware_concurrency();
  }

  if (isVerbose()) {
    std::cout << "Training parallel using " << nthreadToUse << " threads"
              << std::endl;
  }

  size_t splitSampleSize = (size_t) (getSplitRatio() * sampSize);
  size_t averageSampleSize = sampSize - splitSampleSize;

  if (
    (splitSampleSize < 2 * nodeSizeSpt || averageSampleSize < 2 * nodeSizeAvg)
    &&
    (splitRatio != 1 && splitRatio != 0)
  ) {
    throw std::runtime_error("splitRatio too big or too small.");
  }

  #if DOPARELLEL
  std::vector<std::thread> allThreads( nthreadToUse );
  std::mutex threadLock;

  for (size_t t = 0; t < nthreadToUse; t++) {
    auto dummyThread = std::bind(
      [&](const int iStart, const int iEnd, const int t) {

        // loop over all items
        for (int i = iStart; i < iEnd; i++) {
  #else
  for(int i=0; i< ((int) getNtree()); i++ ){
  #endif
          unsigned int myseed = getSeed() * (i + 1);

          std::vector<size_t> sampleIndex;

          if (isReplacement()) {

            while (sampleIndex.size() < getSampleSize()) {
              size_t randomIndex = (size_t)
                      (rand_r(&myseed) %
                       ((int) (*getTrainingData()).getNumRows()));
              sampleIndex.push_back(randomIndex);
            }

          } else {

            while (sampleIndex.size() < getSampleSize()) {
              size_t randomIndex = (size_t)
                      (rand_r(&myseed) %
                       ((int) (*getTrainingData()).getNumRows()));
              if (
                      sampleIndex.size() == 0 ||
                      std::find(
                              sampleIndex.begin(),
                              sampleIndex.end(),
                              randomIndex
                      ) == sampleIndex.end()
                      ) {
                sampleIndex.push_back(randomIndex);
              }
            }

          }

          std::unique_ptr<std::vector<size_t> > splitSampleIndex;
          std::unique_ptr<std::vector<size_t> > averageSampleIndex;

          if (splitRatio == 1 || splitRatio == 0) {

            // Treat it as normal RF
            splitSampleIndex.reset(new std::vector<size_t>(sampleIndex));
            averageSampleIndex.reset(new std::vector<size_t>(sampleIndex));

          } else {

            // Generate sample index
            std::vector<size_t> splitSampleIndex_;
            std::vector<size_t> averageSampleIndex_;

            for (
                    std::vector<size_t>::iterator it = sampleIndex.begin();
                    it != sampleIndex.end();
                    ++it
                    ) {
              if (splitSampleIndex_.size() < splitSampleSize) {
                splitSampleIndex_.push_back(*it);
              } else {
                averageSampleIndex_.push_back(*it);
              }
            }

            splitSampleIndex.reset(
                    new std::vector<size_t>(splitSampleIndex_)
            );
            averageSampleIndex.reset(
                    new std::vector<size_t>(averageSampleIndex_)
            );

          }

          try{
            honestRFTree *oneTree(new honestRFTree(
                    getTrainingData(), getMtry(),
                    getNodeSizeSpt(), getNodeSizeAvg(),
                    std::move(splitSampleIndex),
                    std::move(averageSampleIndex),
                    myseed
            ));

            #if DOPARELLEL
                  std::lock_guard<std::mutex> lock(threadLock);
            #endif

            if (isVerbose()) {
              std::cout << "Finish training tree # " << (i + 1) << std::endl;
            }

            (*forest).emplace_back(oneTree);

          } catch (std::runtime_error &err) {
            std::cerr << err.what() << std::endl;
          }

        }
  #if DOPARELLEL
      },
      t * getNtree() / nthreadToUse,
      (t + 1) == nthreadToUse ? getNtree() : (t + 1) * getNtree() / nthreadToUse,
      t
    );

    allThreads[t] = std::thread(dummyThread);
  }

  std::for_each(
    allThreads.begin(),
    allThreads.end(),
    [](std::thread& x){x.join();}
  );
  #endif

  this->_forest = std::move(forest);
}

std::unique_ptr< std::vector<double> > honestRF::predict(
  std::vector< std::vector<double> >* xNew
){

  std::vector<double> prediction;
  size_t numObservations = (*xNew)[0].size();
  for (size_t j=0; j<numObservations; j++) {
    prediction.push_back(0);
  }

  #if DOPARELLEL
  size_t nthreadToUse = getNthread();
  if (getNthread() == 0) {
    // Use all threads
    nthreadToUse = std::thread::hardware_concurrency();
  }
//  std::cout << "Parallel using " << nthreadToUse << " threads..." << std::endl;

  std::vector<std::thread> allThreads(nthreadToUse);
  std::mutex threadLock;

  for (size_t t = 0; t < nthreadToUse; t++) {
    auto dummyThread = std::bind(
      [&](const int iStart, const int iEnd, const int t) {

        // loop over all items
        for (int i=iStart; i < iEnd; i++) {
  #else
  for(int i=0; i<((int) getNtree()); i++ ) {
  #endif
          try {
            std::vector<double> currentTreePrediction(numObservations);
            honestRFTree *currentTree = (*getForest())[i].get();
            (*currentTree).predict(
                    currentTreePrediction,
                    xNew,
                    getTrainingData()
            );

          #if DOPARELLEL
            std::lock_guard<std::mutex> lock(threadLock);
          #endif

            for (size_t j = 0; j < numObservations; j++) {
              prediction[j] += currentTreePrediction[j];
            }

          } catch (std::runtime_error &err) {
            std::cerr << err.what() << std::endl;
          }
      }
  #if DOPARELLEL
      },
      t * getNtree() / nthreadToUse,
      (t + 1) == nthreadToUse ? getNtree() : (t + 1) * getNtree() / nthreadToUse,
      t
    );
    allThreads[t] = std::thread(dummyThread);
  }

  std::for_each(
    allThreads.begin(),
    allThreads.end(),
    [](std::thread& x) { x.join(); }
  );
  #endif

  for (size_t j=0; j<numObservations; j++){
    prediction[j] /= getNtree();
  }

  std::unique_ptr< std::vector<double> > prediction_ (
    new std::vector<double>(prediction)
  );

  return prediction_;
}

void honestRF::calculateOOBError() {

  size_t numObservations = getTrainingData()->getNumRows();

  std::vector<double> outputOOBPrediction(numObservations);
  std::vector<size_t> outputOOBCount(numObservations);

  for (size_t i=0; i<numObservations; i++) {
    outputOOBPrediction[i] = 0;
    outputOOBCount[i] = 0;
  }

  #if DOPARELLEL
  size_t nthreadToUse = getNthread();
  if (nthreadToUse == 0) {
    // Use all threads
    nthreadToUse = std::thread::hardware_concurrency();
  }
  std::vector<std::thread> allThreads(nthreadToUse);
  std::mutex threadLock;

  for (size_t t = 0; t < nthreadToUse; t++) {
    auto dummyThread = std::bind(
      [&](const int iStart, const int iEnd, const int t) {

        // loop over all items
        for (int i=iStart; i < iEnd; i++) {
  #else
  for (int i=0; i<((int) getNtree()); i++ ) {
  #endif
          try {
            std::vector<double> outputOOBPrediction_iteration(numObservations);
            std::vector<size_t> outputOOBCount_iteration(numObservations);
            for (size_t j=0; j<numObservations; j++) {
              outputOOBPrediction_iteration[j] = 0;
              outputOOBCount_iteration[j] = 0;
            }
            honestRFTree *currentTree = (*getForest())[i].get();
            (*currentTree).getOOBPrediction(
              outputOOBPrediction_iteration,
              outputOOBCount_iteration,
              getTrainingData()
            );

          #if DOPARELLEL
            std::lock_guard<std::mutex> lock(threadLock);
          #endif

            for (size_t j=0; j < numObservations; j++) {
              outputOOBPrediction[j] += outputOOBPrediction_iteration[j];
              outputOOBCount[j] += outputOOBCount_iteration[j];
            }

          } catch (std::runtime_error &err) {
            std::cerr << err.what() << std::endl;
          }
        }
#if DOPARELLEL
        },
        t * getNtree() / nthreadToUse,
        (t + 1) == nthreadToUse ? getNtree() : (t + 1) * getNtree() / nthreadToUse,
        t
    );
    allThreads[t] = std::thread(dummyThread);
  }

  std::for_each(
    allThreads.begin(),
    allThreads.end(),
    [](std::thread& x) { x.join(); }
  );
  #endif

  double OOB_MSE = 0;
  for (size_t j=0; j<numObservations; j++){
    double trueValue = getTrainingData()->getOutcomePoint(j);
    if (outputOOBCount[j] != 0) {
      OOB_MSE +=
        pow(trueValue - outputOOBPrediction[j] / outputOOBCount[j], 2);
    }
  }

  this->_OOBError = OOB_MSE;
};