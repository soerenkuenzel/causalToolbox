#ifndef HTECPP_RFNODE_H
#define HTECPP_RFNODE_H

#include <iostream>
#include <vector>
#include <string>
#include "DataFrame.h"

class RFNode {
public:
  RFNode();
  virtual ~RFNode();

  RFNode(
    std::vector<size_t>* averagingSampleIndex,
    std::vector<size_t>* splittingSampleIndex
  );

  RFNode(
    size_t splitFeature,
    double splitValue,
    RFNode* leftChild,
    RFNode* rightChild
  );

  void setLeafNode(
    std::vector<size_t>* averagingSampleIndex,
    std::vector<size_t>* splittingSampleIndex
  );

  void setSplitNode(
    size_t splitFeature,
    double splitValue,
    RFNode* leftChild,
    RFNode* rightChild
  );

  void predict(
    std::vector<double> &outputPrediction,
    std::vector<size_t>* updateIndex,
    std::vector< std::vector<double> >* xNew,
    DataFrame* trainingData
  );

  bool is_leaf();

  void printSubtree(int indentSpace=0);

  size_t getSplitFeature(){
    if (is_leaf()) {
      throw "Cannot get split feature for a leaf.";
    } else {
      return _splitFeature;
    }

  }

  double getSplitValue(){
    if (is_leaf()) {
      throw "Cannot get split feature for a leaf.";
    } else {
      return _splitValue;
    }
  }

  RFNode* getLeftChild(){
    if (is_leaf()) {
      throw "Cannot get left child for a leaf.";
    } else {
      return _leftChild;
    }
  }

  RFNode* getRightChild(){
    if (is_leaf()) {
      throw "Cannot get right child for a leaf.";
    } else {
      return _rightChild;
    }
  }

  size_t getSplitCount(){
    return _splitCount;
  }

  size_t getAverageCount(){
    return _averageCount;
  }

  std::vector<size_t>* getAveragingIndex(){
    return _averagingSampleIndex;
  }

private:
  std::vector<size_t>* _averagingSampleIndex;
  std::vector<size_t>* _splittingSampleIndex;
  size_t _splitFeature;
  double _splitValue;
  RFNode* _leftChild;
  RFNode* _rightChild;
  size_t _averageCount;
  size_t _splitCount;
};


#endif //HTECPP_RFNODE_H
