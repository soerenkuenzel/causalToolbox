#ifndef HTECPP_RFTREE_H
#define HTECPP_RFTREE_H

#include <iostream>
#include <vector>
#include <string>
#include <random>
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
    std::unique_ptr< std::vector<size_t> > splittingSampleIndex,
    std::unique_ptr< std::vector<size_t> > averagingSampleIndex,
    std::mt19937_64& random_number_generator,
    bool splitMiddle
  );

  // This tree is only for testing purpose
  void setDummyTree(
    size_t mtry,
    size_t nodeSizeSpt,
    size_t nodeSizeAvg,
    std::unique_ptr< std::vector<size_t> > splittingSampleIndex,
    std::unique_ptr< std::vector<size_t> > averagingSampleIndex
  );

  void predict(
    std::vector<double> &outputPrediction,
    std::vector< std::vector<double> >* xNew,
    DataFrame* trainingData
  );

  void recursivePartition(
    RFNode* rootNode,
    std::vector<size_t>* averagingSampleIndex,
    std::vector<size_t>* splittingSampleIndex,
    DataFrame* trainingData,
    std::mt19937_64& random_number_generator,
    bool splitMiddle
  );

  void selectBestFeature(
    size_t& bestSplitFeature,
    double& bestSplitValue,
    double& bestSplitLoss,
    std::vector<size_t>* featureList,
    std::vector<size_t>* averagingSampleIndex,
    std::vector<size_t>* splittingSampleIndex,
    DataFrame* trainingData,
    std::mt19937_64& random_number_generator,
    bool splitMiddle
  );

  void printTree();

  void getOOBindex(
    std::vector<size_t> &outputOOBIndex,
    size_t nRows
  );

  void getOOBPrediction(
    std::vector<double> &outputOOBPrediction,
    std::vector<size_t> &outputOOBCount,
    DataFrame* trainingData
  );

  size_t getMtry() {
    return _mtry;
  }

  size_t getNodeSizeSpt() {
    return _nodeSizeSpt;
  }

  size_t getNodeSizeAvg() {
    return _nodeSizeAvg;
  }

  std::vector<size_t>* getSplittingIndex() {
    return _averagingSampleIndex.get();
  }

  std::vector<size_t>* getAveragingIndex() {
    return _splittingSampleIndex.get();
  }

  RFNode* getRoot() {
    return _root.get();
  }

private:
  size_t _mtry;
  size_t _nodeSizeSpt;
  size_t _nodeSizeAvg;
  std::unique_ptr< std::vector<size_t> > _averagingSampleIndex;
  std::unique_ptr< std::vector<size_t> > _splittingSampleIndex;
  std::unique_ptr< RFNode > _root;
};


#endif //HTECPP_RFTREE_H
