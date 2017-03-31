#ifndef HTECPP_RFTREE_H
#define HTECPP_RFTREE_H

#include <iostream>
#include <vector>
#include <string>
#include "DataFrame.h"
#include "RFNode.h"

class honestRFTree {
public:
  honestRFTree();
  virtual ~honestRFTree();

  honestRFTree(
    DataFrame* trainingData,
    size_t mtry,
    size_t nodeSizeSpt,
    size_t nodeSizeAvg,
    std::vector<size_t>* averagingSampleIndex,
    std::vector<size_t>* splittingSampleIndex,
    unsigned int myseed
  );

  // This tree is only for testing purpose
  void setDummyTree(
    DataFrame* trainingData,
    size_t mtry,
    size_t nodeSizeSpt,
    size_t nodeSizeAvg,
    std::vector<size_t>* averagingSampleIndex,
    std::vector<size_t>* splittingSampleIndex
  );

  void predict(
    std::vector<double> &outputPrediction,
    std::vector< std::vector<double> >* xNew
  );

  void recursivePartition(
    RFNode &rootNode,
    std::vector<size_t>* averagingSampleIndex,
    std::vector<size_t>* splittingSampleIndex,
    unsigned int myseed
  );

  void selectBestFeature(
    size_t& bestSplitFeature,
    double& bestSplitValue,
    double& bestSplitLoss,
    std::vector<size_t>* featureList,
    std::vector<size_t>* averagingSampleIndex,
    std::vector<size_t>* splittingSampleIndex,
    unsigned int myseed
  );

  void printTree();

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

  std::vector<size_t>* getSplittingIndex(){
    return _averagingSampleIndex;
  }

  std::vector<size_t>* getAveragingIndex(){
    return _splittingSampleIndex;
  }

  RFNode* getRoot(){
    return _root;
  }

private:
  DataFrame* _trainingData;
  size_t _mtry;
  size_t _nodeSizeSpt;
  size_t _nodeSizeAvg;
  std::vector<size_t>* _averagingSampleIndex;
  std::vector<size_t>* _splittingSampleIndex;
  RFNode* _root;
};


#endif //HTECPP_RFTREE_H
