#ifndef HTECPP_RF_H
#define HTECPP_RF_H

#include <iostream>
#include <vector>
#include <string>
#include "DataFrame.h"
#include "honestRFTree.h"

class honestRF {

public:
  honestRF();
  virtual ~honestRF();

  honestRF(
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
  );

  std::unique_ptr< std::vector<double> > predict(
    std::vector< std::vector<double> >* xNew,
    size_t nthread
  );

  DataFrame* getTrainingData() {
    return _trainingData.get();
  }

  size_t getMtry() {
    return _mtry;
  }

  size_t getNodeSizeSpt() {
    return _nodeSizeSpt;
  }

  size_t getNodeSizeAvg() {
    return _nodeSizeAvg;
  }

  size_t getNtree() {
    return _ntree;
  }

  size_t getSampleSize() {
    return _sampSize;
  }

  double getSplitRatio() {
    return _splitRatio;
  }

  bool isReplacement() {
    return _replace;
  }

  unsigned int getSeed() {
    return _seed;
  }

  std::vector< std::unique_ptr< honestRFTree > >* getForest() {
    return _forest.get();
  }

  bool isVerbose() {
    return _verbose;
  }

private:
  std::unique_ptr<DataFrame> _trainingData;
  size_t _ntree;
  bool _replace;
  size_t _sampSize;
  double _splitRatio;
  size_t _mtry;
  size_t _nodeSizeSpt;
  size_t _nodeSizeAvg;
  std::unique_ptr< std::vector< std::unique_ptr< honestRFTree > > > _forest;
  unsigned int _seed;
  bool _verbose;
};

#endif //HTECPP_RF_H
