#include "honestRF.h"
#include <random>
#include <thread>
#include <mutex>

honestRF::honestRF():
  _trainingData(nullptr), _ntree(0), _replace(0), _sampSize(0),
  _splitRatio(0), _mtry(0), _nodeSizeSpt(0), _nodeSizeAvg(0),
  _forest(nullptr), _seed(0), _verbose(0){};

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
  this->_verbose = verbose;

  if (splitRatio > 1 || splitRatio < 0) {
    throw "splitRatio shoule be between 0 and 1.";
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
  std::vector<std::thread> allThreads( nthreadToUse );
  std::mutex threadLock;

  for (size_t t = 0; t < nthreadToUse; t++) {
    auto dummyThread = std::bind(
      [&](const int iStart, const int iEnd, const int t) {
        // loop over all items
        for (int i = iStart; i < iEnd; i++) {
//  for(int i=0; i<getNtree(); i++ ){
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

          std::unique_ptr< std::vector<size_t> > splitSampleIndex;
          std::unique_ptr< std::vector<size_t> > averageSampleIndex;

          if (splitRatio == 1 || splitRatio == 0) {

            // Treat it as normal RF
            splitSampleIndex.reset( new std::vector<size_t>(sampleIndex) );
            averageSampleIndex.reset( new std::vector<size_t>(sampleIndex) );

          } else {

            size_t splitSampleSize = (size_t) (getSplitRatio() * sampSize);
            size_t averageSampleSize = sampSize - splitSampleSize;

            if (
              splitSampleSize < 2 * nodeSizeSpt ||
              averageSampleSize < 2 * nodeSizeAvg
            ) {
              throw "splitRatio too big or too small.";
            }

            // Generate sample index
            std::vector<size_t> splitSampleIndex_;
            std::vector<size_t> averageSampleIndex_;

            for (
              std::vector<size_t>::iterator it=sampleIndex.begin();
              it!=sampleIndex.end();
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

          honestRFTree* oneTree ( new honestRFTree(
            getTrainingData(), getMtry(),
            getNodeSizeSpt(), getNodeSizeAvg(),
            std::move(splitSampleIndex), std::move(averageSampleIndex),
            myseed
          ));

          std::lock_guard<std::mutex> lock(threadLock);

          if (isVerbose()) {
            std::cout << "Finish training tree # " << (i + 1) << std::endl;
          }

          (*forest).emplace_back(oneTree);

        }
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

  this->_forest = std::move(forest);
}

std::unique_ptr< std::vector<double> > honestRF::predict(
  std::vector< std::vector<double> >* xNew,
  size_t nthread
){

  std::vector<double> prediction;
  size_t numObservations = (*xNew)[0].size();
  for (size_t j=0; j<numObservations; j++) {
    prediction.push_back(0);
  }

  size_t nthreadToUse = nthread;
  if (nthread == 0) {
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
//  for(int i=0; i<getNtree(); i++ ){
          std::vector<double> currentTreePrediction(numObservations);
          honestRFTree* currentTree = (*getForest())[i].get();
          (*currentTree).predict(
            currentTreePrediction,
            xNew,
            getTrainingData()
          );
          std::lock_guard<std::mutex> lock(threadLock);
          for (size_t j=0; j<numObservations; j++){
            prediction[j] += currentTreePrediction[j];
          }
        }
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

  for (size_t j=0; j<numObservations; j++){
    prediction[j] /= getNtree();
  }

  std::unique_ptr< std::vector<double> > prediction_ (
    new std::vector<double>(prediction)
  );

  return prediction_;
}
