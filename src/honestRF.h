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
    unsigned int seed
  );

  std::vector<double>* predict(
    std::vector<std::vector<double>>* xNew
  );

  DataFrame* getTrainingData(){
    return _trainingData;
  }

  size_t getMtry(){
    return _mtry;
  }

  size_t getNodeSizeSpt(){
    return _nodeSizeSpt;
  }

  size_t getNodeSizeAvg(){
    return _nodeSizeAvg;
  }

  size_t getNtree(){
    return _ntree;
  }

  size_t getSampleSize(){
    return _sampSize;
  }

  double getSplitRatio(){
    return _splitRatio;
  }

  bool isReplacement(){
    return _replace;
  }

  unsigned int getSeed(){
    return _seed;
  }

  std::vector<honestRFTree>* getForest(){
    return _forest;
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
  std::vector<honestRFTree>* _forest;
  unsigned int _seed;
};

#endif //HTECPP_RF_H
