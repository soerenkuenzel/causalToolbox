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
    DataFrame* trainingData,
    size_t ntree,
    bool replace,
    size_t sampSize,
    double splitRatio,
    size_t mtry,
    size_t nodeSizeSpt,
    size_t nodeSizeAvg,
    unsigned int seed,
    size_t nthread,
    bool verbose,
    bool splitMiddle
  );

  std::unique_ptr< std::vector<double> > predict(
    std::vector< std::vector<double> >* xNew
  );

  void calculateOOBError();

  double getOOBError() {
    calculateOOBError();
    return _OOBError;
  }

  void addTrees(size_t ntree);

  DataFrame* getTrainingData() {
    return _trainingData;
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

  size_t getNthread(){
    return _nthread;
  }

  bool getSplitMiddle(){
    return _splitMiddle;
  }

private:
  DataFrame* _trainingData;
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
  size_t _nthread;
  double _OOBError;
  bool _splitMiddle;
};

#endif //HTECPP_RF_H
