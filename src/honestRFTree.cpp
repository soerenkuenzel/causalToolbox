#include "honestRFTree.h"
#include "RFNode.h"
#include <math.h>
#include <set>
#include <map>
#include <tuple>

honestRFTree::honestRFTree():
  _trainingData(0), _mtry(0), _nodeSizeSpt(0), _nodeSizeAvg(0),
  _averagingSampleIndex(0), _splittingSampleIndex(0), _root(0) {};

honestRFTree::~honestRFTree(){};

honestRFTree::honestRFTree(
  DataFrame* trainingData,
  size_t mtry,
  size_t nodeSizeSpt,
  size_t nodeSizeAvg,
  std::vector<size_t>* averagingSampleIndex,
  std::vector<size_t>* splittingSampleIndex
){
  if (nodeSizeAvg == 0 || nodeSizeSpt == 0) {
    throw "nodeSize cannot be set to 0.";
  }

  if (nodeSizeAvg > (* averagingSampleIndex).size() ||
          nodeSizeSpt > (* splittingSampleIndex).size()){
    throw "nodeSize cannot exceed total elements in the sample.";
  }

  if ((* averagingSampleIndex).size() == 0 ||
          (* splittingSampleIndex).size() == 0){
    throw "sample size cannot be 0.";
  }

  if (mtry == 0 | mtry > (*trainingData).getNumColumns()) {
    throw "mtry must be positive and cannot exceed total amount of features";
  }

  this->_trainingData = trainingData;
  this->_mtry = mtry;
  this->_nodeSizeSpt = nodeSizeSpt;
  this->_nodeSizeAvg = nodeSizeAvg;
  this->_averagingSampleIndex = averagingSampleIndex;
  this->_splittingSampleIndex = splittingSampleIndex;
  RFNode *root = new RFNode();
  this->_root = root;
  recursivePartition(*root, getAveragingIndex(),
                     getSplittingIndex());
}

void honestRFTree::setDummyTree(
  DataFrame* trainingData,
  size_t mtry,
  size_t nodeSizeSpt,
  size_t nodeSizeAvg,
  std::vector<size_t>* averagingSampleIndex,
  std::vector<size_t>* splittingSampleIndex
){
  _trainingData = trainingData;
  _mtry = mtry;
  _nodeSizeSpt = nodeSizeSpt;
  _nodeSizeAvg = nodeSizeAvg;
  _averagingSampleIndex = averagingSampleIndex;
  _splittingSampleIndex = splittingSampleIndex;
}

void honestRFTree::predict(
  std::vector<double> &outputPrediction,
  std::vector<std::vector<double>>* xNew
){

  struct rangeGenerator {
    size_t currentNumber;
    rangeGenerator(size_t startNumber): currentNumber(startNumber){};
    size_t operator()(){return currentNumber++;}
  };
  std::vector<size_t> updateIndex(outputPrediction.size());
  rangeGenerator _rangeGenerator(0);
  std::generate(updateIndex.begin(),
                updateIndex.end(),
                _rangeGenerator);
  (*getRoot()).predict(outputPrediction, &updateIndex, xNew, getTrainingData());
}

void honestRFTree::recursivePartition(
  RFNode &rootNode,
  std::vector<size_t>* averagingSampleIndex,
  std::vector<size_t>* splittingSampleIndex
){
  // Sample mtry amounts of features
  std::vector<size_t> featureList;

  while (featureList.size() < getMtry()){
    size_t randomIndex = (size_t) (rand() %
            ((int) (*getTrainingData()).getNumColumns()));
    if (featureList.size() == 0 ||
        std::find(featureList.begin(),
                  featureList.end(), randomIndex) == featureList.end()){
      featureList.push_back(randomIndex);
    }
  }

  // Select best feature
  size_t bestSplitFeature;
  double bestSplitValue;
  double bestSplitLoss;
  selectBestFeature(bestSplitFeature, bestSplitValue, bestSplitLoss,
                    &featureList, averagingSampleIndex, splittingSampleIndex);

  // Create a leaf node if the current bestSplitValue is NA
  if (isnan(bestSplitValue)) {
    std::vector<size_t> *averagingSampleIndex_ =
            new std::vector<size_t>(*averagingSampleIndex);
    std::vector<size_t> *splittingSampleIndex_ =
            new std::vector<size_t>(*splittingSampleIndex);
    rootNode.setLeafNode(averagingSampleIndex_, splittingSampleIndex_);
  } else {

    // Test if the current feature is categorical
    std::vector<size_t> averagingLeftPartitionIndex;
    std::vector<size_t> averagingRightPartitionIndex;
    std::vector<size_t> splittingLeftPartitionIndex;
    std::vector<size_t> splittingRightPartitionIndex;
    std::vector<size_t> categorialCols = *(*getTrainingData()).getCatCols();
    if (std::find(
            categorialCols.begin(),
            categorialCols.end(),
            bestSplitFeature) != categorialCols.end()){
      for(std::vector<size_t>::iterator it = (*averagingSampleIndex).begin();
          it != (*averagingSampleIndex).end();
          ++it) {
        if ((*getTrainingData()).getPoint(*it, bestSplitFeature)
            == bestSplitValue) {
          averagingLeftPartitionIndex.push_back(*it);
        } else {
          averagingRightPartitionIndex.push_back(*it);
        }
      }
      for(std::vector<size_t>::iterator it = (*splittingSampleIndex).begin();
          it != (*splittingSampleIndex).end();
          ++it) {
        if ((*getTrainingData()).getPoint(*it, bestSplitFeature)
            == bestSplitValue) {
          splittingLeftPartitionIndex.push_back(*it);
        } else {
          splittingRightPartitionIndex.push_back(*it);
        }
      }
    } else {
      // For non-categorical, split left and right according to the split point
      for (std::vector<size_t>::iterator it = (*averagingSampleIndex).begin();
           it != (*averagingSampleIndex).end();
           ++it) {
        if ((*getTrainingData()).getPoint(*it, bestSplitFeature)
            < bestSplitValue) {
          averagingLeftPartitionIndex.push_back(*it);
        } else {
          averagingRightPartitionIndex.push_back(*it);
        }
      }
      for (std::vector<size_t>::iterator it = (*splittingSampleIndex).begin();
           it != (*splittingSampleIndex).end();
           ++it) {
        if ((*getTrainingData()).getPoint(*it, bestSplitFeature)
            < bestSplitValue) {
          splittingLeftPartitionIndex.push_back(*it);
        } else {
          splittingRightPartitionIndex.push_back(*it);
        }
      }
    }
    // Update sample index for both left and right partitions
    // Recursively grow the tree
    RFNode* leftChild = new RFNode();
    RFNode* rightChild = new RFNode();

//    std::cout << "Creating left=" << leftChild << " right=" << rightChild << " from " << &rootNode << std::endl;
//    std::cout << "Preparing splitting " << bestSplitFeature << ' ' << bestSplitValue << ' ' << bestSplitLoss <<std::endl;
//    std::cout << "Preparing to create left child: #split " << splittingLeftPartitionIndex.size() << " #average" << averagingLeftPartitionIndex.size() << std::endl;
//    std::cout << "Preparing to create right child: #split " << splittingRightPartitionIndex.size() << " #average" << averagingRightPartitionIndex.size() << std::endl;
//    std::cout << "========" << std::endl;
    recursivePartition(*leftChild, &averagingLeftPartitionIndex,
                       &splittingLeftPartitionIndex);
    recursivePartition(*rightChild, &averagingRightPartitionIndex,
                       &splittingRightPartitionIndex);
//    std::cout << "Preparing merging " << bestSplitFeature << ' ' << bestSplitValue << std::endl;
    // Create the leaf node the connects to both children
//    std::cout << "Merging left=" << leftChild << " right=" << rightChild << " to " << &rootNode << std::endl;
    rootNode.setSplitNode(bestSplitFeature, bestSplitValue,
                          leftChild, rightChild);
  }
}

void honestRFTree::selectBestFeature(
  size_t &bestSplitFeature,
  double &bestSplitValue,
  double &bestSplitLoss,
  std::vector<size_t>* featureList,
  std::vector<size_t>* averagingSampleIndex,
  std::vector<size_t>* splittingSampleIndex
){
  // Get the number of total features
  size_t mtry = (*featureList).size();

  // Initialize the minimum loss for each feature
  double* bestSplitLossAll = new double[mtry];
  double* bestSplitValueAll = new double[mtry];
  size_t* bestSplitFeatureAll = new size_t[mtry];
  size_t* bestSplitCountAll = new size_t[mtry];

  for (int i=0; i<mtry; i++) {
    bestSplitLossAll[i] = -std::numeric_limits<double>::infinity();
    bestSplitValueAll[i] = std::numeric_limits<double>::quiet_NaN();
    bestSplitFeatureAll[i] = std::numeric_limits<size_t>::quiet_NaN();
    bestSplitCountAll[i] = 0;
  }

  // Iterate each selected features
  for (int i=0; i<mtry; i++) {
    size_t currentFeature = (*featureList)[i];
    // Test if the current feature is in the categorical list
    std::vector<size_t> categorialCols = *(*getTrainingData()).getCatCols();
    if (std::find(
            categorialCols.begin(),
            categorialCols.end(),
            currentFeature) != categorialCols.end()){

      // Count total number of different categories
      std::set<double> all_categories;
      double splitTotalSum = 0;
      size_t splitTotalCount = 0;
      size_t averageTotalCount = 0;
      for (int j=0; j<(*splittingSampleIndex).size(); j++){
        all_categories.insert(
                (*getTrainingData()).getPoint((*splittingSampleIndex)[j],
                                              currentFeature));
        splitTotalSum += (*getTrainingData()).
                getOutcomePoint((*splittingSampleIndex)[j]);
        splitTotalCount++;
      }
      for (int j=0; j<(*averagingSampleIndex).size(); j++){
        all_categories.insert(
                (*getTrainingData()).getPoint((*averagingSampleIndex)[j],
                                              currentFeature));
        averageTotalCount++;
      }

      // Create map to track the count and sum of y squares
      std::map<double, size_t> splittingCategoryCount;
      std::map<double, size_t> averagingCategoryCount;
      std::map<double, double> splittingCategoryYSum;

      for (std::set<double>::iterator it=all_categories.begin();
           it != all_categories.end(); ++it){
        splittingCategoryCount[*it] = 0;
        averagingCategoryCount[*it] = 0;
        splittingCategoryYSum[*it] = 0;
      }

      for (int j=0; j<(*splittingSampleIndex).size(); j++){
        double currentXValue = (*getTrainingData()).
                getPoint((*splittingSampleIndex)[j], currentFeature);
        double currentYValue = (*getTrainingData()).
                getOutcomePoint((*splittingSampleIndex)[j]);
        splittingCategoryCount[currentXValue] += 1;
        splittingCategoryYSum[currentXValue] += currentYValue;
      }

      for (int j=0; j<(*averagingSampleIndex).size(); j++){
        double currentXValue = (*getTrainingData()).
                getPoint((*averagingSampleIndex)[j], currentFeature);
        averagingCategoryCount[currentXValue] += 1;
      }

      // Go through the sums and determine the best partition
      for (std::set<double>::iterator it=all_categories.begin();
           it != all_categories.end(); ++it){
        // Check leaf size at least nodesize
        if (
          std::min(
            splittingCategoryCount[*it],
            splitTotalCount - splittingCategoryCount[*it]
          ) < getNodeSizeSpt()||
          std::min(
            averagingCategoryCount[*it],
            averageTotalCount - averagingCategoryCount[*it]
          ) < getNodeSizeAvg()
        ){
          continue;
        }

        double leftPartitionMean = splittingCategoryYSum[*it] /
                splittingCategoryCount[*it];
        double rightPartitionMean = (splitTotalSum -
                splittingCategoryYSum[*it]) /
                (splitTotalCount - splittingCategoryCount[*it]);
        double currentSplitLoss = splittingCategoryCount[*it] *
                leftPartitionMean * leftPartitionMean +
                (splitTotalCount - splittingCategoryCount[*it]) *
                rightPartitionMean * rightPartitionMean;

        // Update the value if a higher value has been seen
        if (currentSplitLoss > bestSplitLossAll[i]) {
          bestSplitLossAll[i] = currentSplitLoss;
          bestSplitFeatureAll[i] = currentFeature;
          bestSplitValueAll[i] = *it;
          bestSplitCountAll[i] = 1;
        } else {
          //If we are as good as the best split
          if (currentSplitLoss == bestSplitLossAll[i]) {
            bestSplitCountAll[i] = bestSplitCountAll[i] + 1;
            // Only update with probability 1/nseen
            double tmp_random = (double) rand() / RAND_MAX;
            if (tmp_random * bestSplitCountAll[i] <= 1) {
              bestSplitLossAll[i] = currentSplitLoss;
              bestSplitFeatureAll[i] = currentFeature;
              bestSplitValueAll[i] = *it;
            }
          }
        }
      }
      continue;
    }

    // Create specific vectors to holddata
    typedef std::tuple<double,double> dataPair;
    std::vector<dataPair> splittingData;
    std::vector<dataPair> averagingData;
    double splitTotalSum = 0;
    for (int j=0; j<(*splittingSampleIndex).size(); j++){
      // Retrieve the current feature value
      double tmpFeatureValue = (*getTrainingData()).
              getPoint((*splittingSampleIndex)[j], currentFeature);
      double tmpOutcomeValue = (*getTrainingData()).
              getOutcomePoint((*splittingSampleIndex)[j]);
      splitTotalSum += tmpOutcomeValue;

      // Adding data to the internal data vector (Note: R index)
      splittingData.push_back(
        std::make_tuple(
          tmpFeatureValue,
          tmpOutcomeValue
        )
      );
    }

    for (int j=0; j<(*averagingSampleIndex).size(); j++){
      // Retrieve the current feature value
      double tmpFeatureValue = (*getTrainingData()).
              getPoint((*averagingSampleIndex)[j], currentFeature);
      double tmpOutcomeValue = (*getTrainingData()).
              getOutcomePoint((*averagingSampleIndex)[j]);

      // Adding data to the internal data vector (Note: R index)
      averagingData.push_back(
        std::make_tuple(
          tmpFeatureValue,
          tmpOutcomeValue
        )
      );
    }

    // Sort both splitting and averaging dataset
    sort(
      splittingData.begin(),
      splittingData.end(),
      [](const dataPair &lhs, const dataPair &rhs) {
        return std::get<0>(lhs) < std::get<0>(rhs);
      }
    );
    sort(
      averagingData.begin(),
      averagingData.end(),
      [](const dataPair &lhs, const dataPair &rhs) {
        return std::get<0>(lhs) < std::get<0>(rhs);
      }
    );

    size_t splitLeftPartitionCount = 0;
    size_t averageLeftPartitionCount = 0;
    size_t splitTotalCount = splittingData.size();
    size_t averageTotalCount = averagingData.size();

    double splitLeftPartitionRunningSum = 0;

    std::vector<dataPair>::iterator splittingDataIter = splittingData.begin();
    std::vector<dataPair>::iterator averagingDataIter = averagingData.begin();

    // Initialize the split value to be minimum of first value in two datsets
    double featureValue = std::min(
      std::get<0>(*splittingDataIter),
      std::get<0>(*averagingDataIter)
    );

    double newFeatureValue;
    bool oneValueDistinctFlag = true;

    while (splittingDataIter < splittingData.end() ||
           averagingDataIter < averagingData.end()){

      // Exhaust all current feature value in both dataset as partitioning
      while (splittingDataIter < splittingData.end() &&
             std::get<0>(*splittingDataIter) == featureValue) {
        splitLeftPartitionCount++;
        splitLeftPartitionRunningSum += std::get<1>(*splittingDataIter);
        splittingDataIter++;
      }
      while (averagingDataIter < averagingData.end() &&
             std::get<0>(*averagingDataIter) == featureValue) {
        averagingDataIter++;
        averageLeftPartitionCount++;
      }

      // Test if the all the values for the feature are the same, then proceed
      if (oneValueDistinctFlag){
        oneValueDistinctFlag = false;
        if (splittingDataIter == splittingData.end() &&
            averagingDataIter == averagingData.end()) {
          break;
        }
      }

      // Make partitions on the current feature and value in both splitting
      // and averaging dataset. `averageLeftPartitionCount` and
      // `splitLeftPartitionCount` already did the partition after we sort the
      // array.
      // Get new feature value
      if (splittingDataIter == splittingData.end() &&
          averagingDataIter == averagingData.end()) {
        break;
      } else if (splittingDataIter == splittingData.end()) {
        newFeatureValue = std::get<0>(*averagingDataIter);
      } else if (averagingDataIter == averagingData.end()) {
        newFeatureValue = std::get<0>(*splittingDataIter);
      } else {
        newFeatureValue = std::min(
          std::get<0>(*splittingDataIter),
          std::get<0>(*averagingDataIter)
        );
      }

      // Check leaf size at least nodesize
      if (
        std::min(
          splitLeftPartitionCount,
          splitTotalCount - splitLeftPartitionCount
        ) < getNodeSizeSpt()||
        std::min(
          averageLeftPartitionCount,
          averageTotalCount - averageLeftPartitionCount
        ) < getNodeSizeAvg()
      ){
        // Update the oldFeature value before proceeding
        featureValue = newFeatureValue;
        continue;
      }

      // Calculate sample mean in both splitting partitions
      double leftPartitionMean =
              splitLeftPartitionRunningSum / splitLeftPartitionCount;
      double rightPartitionMean =
              (splitTotalSum - splitLeftPartitionRunningSum)
              / (splitTotalCount - splitLeftPartitionCount);

      // Calculate the variance of the splitting
      double muBarSquareSum =
              splitLeftPartitionCount * leftPartitionMean * leftPartitionMean +
              (splitTotalCount - splitLeftPartitionCount) * rightPartitionMean
              * rightPartitionMean;

      // Update the value if a higher value has been seen
      if (muBarSquareSum > bestSplitLossAll[i]) {
        bestSplitLossAll[i] = muBarSquareSum;
        bestSplitFeatureAll[i] = currentFeature;
        double tmp_random = (double) rand() / RAND_MAX;
        bestSplitValueAll[i] = tmp_random *
                               (newFeatureValue - featureValue) + featureValue;
        bestSplitCountAll[i] = 1;
      } else {
        //If we are as good as the best split
        if (muBarSquareSum == bestSplitLossAll[i]) {
          bestSplitCountAll[i] = bestSplitCountAll[i] + 1;
          // Only update with probability 1/nseen
          double tmp_random = (double) rand() / RAND_MAX;
          if (tmp_random * bestSplitCountAll[i] <= 1) {
            bestSplitLossAll[i] = muBarSquareSum;
            bestSplitFeatureAll[i] = currentFeature;
            tmp_random = (double) rand() / RAND_MAX;
            bestSplitValueAll[i] = tmp_random * (newFeatureValue
                                                 - featureValue) + featureValue;
          }
        }
      }

      // Update the old feature value
      featureValue = newFeatureValue;
    }
  }

  // Get the best split values among all features
  double bestSplitLoss_ = -std::numeric_limits<double>::infinity();
  std::vector<size_t> bestFeatures;

  for (int i=0; i<mtry; i++) {
    if (bestSplitLossAll[i] > bestSplitLoss_) {
      bestSplitLoss_ = bestSplitLossAll[i];
    }
//    std::cout << bestSplitFeatureAll[i] << ' ' << bestSplitValueAll[i] << ' ' << bestSplitLossAll[i] << ' ' << std::endl;
  }
//  std::cout << std::endl;

  for (size_t i=0; i<mtry; i++) {
    if (bestSplitLossAll[i] == bestSplitLoss_) {
      for (int j=0; j<bestSplitCountAll[i]; j++){
        bestFeatures.push_back(i);
      }
//      std::cout << bestSplitLossAll[i] << std::endl;
    }
  }

  // If we found a feasible splitting point
  if (bestFeatures.size() > 0) {
    // If there are multiple best features, sample one according to their
    // frequency of occurence
    size_t tmp_random = rand() % bestFeatures.size();
    size_t bestFeatureIndex = bestFeatures.at(tmp_random);
    // Return the best splitFeature and splitValue
    bestSplitFeature = bestSplitFeatureAll[bestFeatureIndex];
    bestSplitValue = bestSplitValueAll[bestFeatureIndex];
    bestSplitLoss = bestSplitLoss_;
  }else{
    // If none of the features are possible, return NA
    bestSplitFeature = std::numeric_limits<size_t>::quiet_NaN();
    bestSplitValue = std::numeric_limits<double>::quiet_NaN();
    bestSplitLoss = std::numeric_limits<double>::quiet_NaN();
  }
}

void honestRFTree::printTree(){
  (*getRoot()).printSubtree();
}
