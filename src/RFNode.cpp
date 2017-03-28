#include "RFNode.h"

RFNode::RFNode():
  _averagingSampleIndex(0), _splittingSampleIndex(0), _splitFeature(0),
  _splitValue(0), _leftChild(0), _rightChild(0), _averageCount(0),
  _splitCount(0) {}

RFNode::~RFNode(){};

RFNode::RFNode(
  std::vector<size_t>* averagingSampleIndex,
  std::vector<size_t>* splittingSampleIndex
){
  if ((*averagingSampleIndex).size() == 0 &&
          (*splittingSampleIndex).size() == 0) {
    throw "Intend to create an empty node.";
  }
  // Leaf node constructor
  this->_averagingSampleIndex = averagingSampleIndex;
  this->_averageCount = (*averagingSampleIndex).size();
  this->_splittingSampleIndex = splittingSampleIndex;
  this->_splitCount = (*splittingSampleIndex).size();
}

RFNode::RFNode(
  size_t splitFeature,
  double splitValue,
  RFNode* leftChild,
  RFNode* rightChild
){
  // Split node constructor
  this->_averageCount = 0;
  this->_splitCount = 0;
  this->_splitFeature = splitFeature;
  this->_splitValue = splitValue;
  this->_leftChild = leftChild;
  this->_rightChild = rightChild;
}

void RFNode::setLeafNode(
  std::vector<size_t>* averagingSampleIndex,
  std::vector<size_t>* splittingSampleIndex
){
  _averagingSampleIndex = averagingSampleIndex;
  _averageCount = (*averagingSampleIndex).size();
  _splittingSampleIndex = splittingSampleIndex;
  _splitCount = (*splittingSampleIndex).size();
}

void RFNode::setSplitNode(
  size_t splitFeature,
  double splitValue,
  RFNode* leftChild,
  RFNode* rightChild
){
  // Split node constructor
  _averageCount = 0;
  _splitCount = 0;
  _splitFeature = splitFeature;
  _splitValue = splitValue;
  _leftChild = leftChild;
  _rightChild = rightChild;
}


void RFNode::predict(
  std::vector<double> &outputPrediction,
  std::vector<size_t>* updateIndex,
  std::vector<std::vector<double>>* xNew,
  DataFrame* trainingData
){


  // If the node is a leaf, aggregate all its averaging data samples
  if (is_leaf()) {
//    std::cout << _averageCount << std::endl;
    std::vector<size_t> testhere = *getAveragingIndex();
//    for (auto i=(testhere).begin();
//         i!=(testhere).end();
//         ++i) {
//      std::cout << *i << ' ';
//    }
//    std::cout << std::endl;
    double predictedMean = (*trainingData).partitionMean(getAveragingIndex());
    for(std::vector<size_t>::iterator it = (*updateIndex).begin();
        it != (*updateIndex).end();
        ++it) {
      outputPrediction[*it] = predictedMean;
    }
  } else {
    std::vector<size_t>* leftPartitionIndex = (std::vector<size_t>*)
            malloc(sizeof(std::vector<size_t>));
    std::vector<size_t>* rightPartitionIndex = (std::vector<size_t>*)
            malloc(sizeof(std::vector<size_t>));
    new (leftPartitionIndex) std::vector<size_t>();
    new (rightPartitionIndex) std::vector<size_t>();

    // Test if the splitting feature is categorical
    std::vector<size_t> categorialCols = *(*trainingData).getCatCols();
    if (std::find(
      categorialCols.begin(),
      categorialCols.end(),
      getSplitFeature()) != categorialCols.end()){
      for(std::vector<size_t>::iterator it = (*updateIndex).begin();
          it != (*updateIndex).end();
          ++it) {
        if ((*xNew)[getSplitFeature()][*it] == getSplitValue()) {
          (*leftPartitionIndex).push_back(*it);
        } else {
          (*rightPartitionIndex).push_back(*it);
        }
      }
    } else {
      // For non-categorical, split left and right according to the split point
      for(std::vector<size_t>::iterator it = (*updateIndex).begin();
          it != (*updateIndex).end();
          ++it) {
        if ((*xNew)[getSplitFeature()][*it] < getSplitValue()) {
          (*leftPartitionIndex).push_back(*it);
        } else {
          (*rightPartitionIndex).push_back(*it);
        }
      }
    }

    // Assign predictions from its children
    if ((*leftPartitionIndex).size() > 0) {
      (*getLeftChild()).predict(
        outputPrediction,
        leftPartitionIndex,
        xNew,
        trainingData
      );
    }
    // Assign predictions from the left child
    if ((*rightPartitionIndex).size() > 0) {
      (*getRightChild()).predict(
        outputPrediction,
        rightPartitionIndex,
        xNew,
        trainingData
      );
    }
  }
}

bool RFNode::is_leaf(){
  return !(getAverageCount() == 0 && getSplitCount() == 0);
}

void RFNode::printSubtree(int indentSpace) {
  // Test if the node is leaf node
  if (is_leaf()) {
    // Print count of samples in the leaf node
    std::cout << std::string((unsigned long) indentSpace, ' ')
              << "Leaf Node: # of split samples = "
              << getSplitCount()
              << ", # of average samples = "
              << getAverageCount()
              << std::endl;
  } else {
    // Print split feature and split value
    std::cout << std::string((unsigned long) indentSpace, ' ')
              << "Tree Node: split feature = "
              << getSplitFeature()
              << ", split value = "
              << getSplitValue()
              << std::endl;
    // Recursively calling its children
    (*getLeftChild()).printSubtree(indentSpace+2);
    (*getRightChild()).printSubtree(indentSpace+2);
  }
}
