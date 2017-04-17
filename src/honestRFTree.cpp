#include "honestRFTree.h"
#include <math.h>
#include <set>
#include <map>
#include <random>
// [[Rcpp::plugins(cpp11)]]


honestRFTree::honestRFTree():
  _mtry(0), _nodeSizeSpt(0), _nodeSizeAvg(0), _averagingSampleIndex(nullptr),
  _splittingSampleIndex(nullptr), _root(nullptr) {};

honestRFTree::~honestRFTree() {
//  std::cout << "honestRFTree() destructor is called." << std::endl;
};

honestRFTree::honestRFTree(
  DataFrame* trainingData,
  size_t mtry,
  size_t nodeSizeSpt,
  size_t nodeSizeAvg,
  std::unique_ptr< std::vector<size_t> > splittingSampleIndex,
  std::unique_ptr< std::vector<size_t> > averagingSampleIndex,
  unsigned int myseed
){

  if (nodeSizeAvg == 0 || nodeSizeSpt == 0) {
    throw std::runtime_error("nodeSize cannot be set to 0.");
  }

  if (
    nodeSizeAvg > (*averagingSampleIndex).size() ||
    nodeSizeSpt > (*splittingSampleIndex).size()
  ) {
    throw std::runtime_error("nodeSize cannot exceed total "
                                   "elements in the sample.");
  }

  if (
    (*averagingSampleIndex).size() == 0 ||
    (*splittingSampleIndex).size() == 0
  ) {
    throw std::runtime_error("sample size cannot be 0.");
  }

  if (mtry == 0 || mtry > (*trainingData).getNumColumns()) {
    throw std::runtime_error("mtry must be positive and cannot "
                                   "exceed total amount of features");
  }

  this->_mtry = mtry;
  this->_nodeSizeSpt = nodeSizeSpt;
  this->_nodeSizeAvg = nodeSizeAvg;
  this->_averagingSampleIndex = std::move(averagingSampleIndex);
  this->_splittingSampleIndex = std::move(splittingSampleIndex);
  std::unique_ptr< RFNode > root ( new RFNode() );
  this->_root = std::move(root);
  recursivePartition(
    getRoot(), getAveragingIndex(), getSplittingIndex(), trainingData, myseed
  );
}

void honestRFTree::setDummyTree(
  size_t mtry,
  size_t nodeSizeSpt,
  size_t nodeSizeAvg,
  std::unique_ptr< std::vector<size_t> > splittingSampleIndex,
  std::unique_ptr< std::vector<size_t> > averagingSampleIndex
){
  this->_mtry = mtry;
  this->_nodeSizeSpt = nodeSizeSpt;
  this->_nodeSizeAvg = nodeSizeAvg;
  this->_averagingSampleIndex = std::move(averagingSampleIndex);
  this->_splittingSampleIndex = std::move(splittingSampleIndex);
}

void honestRFTree::predict(
  std::vector<double> &outputPrediction,
  std::vector< std::vector<double> >* xNew,
  DataFrame* trainingData
){

  struct rangeGenerator {
    size_t currentNumber;
    rangeGenerator(size_t startNumber): currentNumber(startNumber) {};
    size_t operator()() {return currentNumber++; }
  };

  std::vector<size_t> updateIndex(outputPrediction.size());
  rangeGenerator _rangeGenerator(0);
  std::generate(updateIndex.begin(), updateIndex.end(), _rangeGenerator);
  (*getRoot()).predict(outputPrediction, &updateIndex, xNew, trainingData);
}

void honestRFTree::recursivePartition(
  RFNode* rootNode,
  std::vector<size_t>* averagingSampleIndex,
  std::vector<size_t>* splittingSampleIndex,
  DataFrame* trainingData,
  unsigned int myseed
){

  // Sample mtry amounts of features
  std::vector<size_t> featureList;
  while (featureList.size() < getMtry()) {
    size_t randomIndex = (size_t) (rand_r(&myseed) %
            ((int) (*trainingData).getNumColumns()));
    if (
      featureList.size() == 0 ||
      std::find(
        featureList.begin(),
        featureList.end(),
        randomIndex
      ) == featureList.end()
    ) {
      featureList.push_back(randomIndex);
    }
  }

  // Select best feature
  size_t bestSplitFeature;
  double bestSplitValue;
  double bestSplitLoss;
  selectBestFeature(
    bestSplitFeature, bestSplitValue, bestSplitLoss, &featureList,
    averagingSampleIndex, splittingSampleIndex, trainingData, myseed
  );

  // Create a leaf node if the current bestSplitValue is NA
  if (std::isnan(bestSplitValue)) {
    // Create two lists on heap and transfer the owernship to the node
    std::unique_ptr< std::vector<size_t> > averagingSampleIndex_ (
      new std::vector<size_t>(*averagingSampleIndex)
    );
    std::unique_ptr< std::vector<size_t> > splittingSampleIndex_ (
      new std::vector<size_t>(*splittingSampleIndex)
    );
    (*rootNode).setLeafNode(
      std::move(averagingSampleIndex_),
      std::move(splittingSampleIndex_)
    );

  } else {
    // Test if the current feature is categorical
    std::vector<size_t> averagingLeftPartitionIndex;
    std::vector<size_t> averagingRightPartitionIndex;
    std::vector<size_t> splittingLeftPartitionIndex;
    std::vector<size_t> splittingRightPartitionIndex;
    std::vector<size_t> categorialCols = *(*trainingData).getCatCols();

    // Create split for both averaging and splitting dataset based on
    // categorical feature or not
    if (
      std::find(
        categorialCols.begin(),
        categorialCols.end(),
        bestSplitFeature
      ) != categorialCols.end()
    ) {

      // categorical, split by (==) or (!=)
      // averaging data
      for (
        std::vector<size_t>::iterator it = (*averagingSampleIndex).begin();
        it != (*averagingSampleIndex).end();
        ++it
      ) {
        if (
          (*trainingData).getPoint(*it, bestSplitFeature) == bestSplitValue
        ) {
          averagingLeftPartitionIndex.push_back(*it);
        } else {
          averagingRightPartitionIndex.push_back(*it);
        }
      }
      // splitting data
      for (
        std::vector<size_t>::iterator it = (*splittingSampleIndex).begin();
        it != (*splittingSampleIndex).end();
        ++it
      ) {
        if (
          (*trainingData).getPoint(*it, bestSplitFeature) == bestSplitValue
        ) {
          splittingLeftPartitionIndex.push_back(*it);
        } else {
          splittingRightPartitionIndex.push_back(*it);
        }
      }

    } else {

      // Non-categorical, split to left (<) and right (>=) according to the
      // split value
      for (
        std::vector<size_t>::iterator it = (*averagingSampleIndex).begin();
        it != (*averagingSampleIndex).end();
        ++it
      ) {
        if (
          (*trainingData).getPoint(*it, bestSplitFeature) < bestSplitValue
        ) {
          averagingLeftPartitionIndex.push_back(*it);
        } else {
          averagingRightPartitionIndex.push_back(*it);
        }
      }
      for (
        std::vector<size_t>::iterator it = (*splittingSampleIndex).begin();
        it != (*splittingSampleIndex).end();
        ++it
      ) {
        if (
          (*trainingData).getPoint(*it, bestSplitFeature) < bestSplitValue
        ) {
          splittingLeftPartitionIndex.push_back(*it);
        } else {
          splittingRightPartitionIndex.push_back(*it);
        }
      }
    }

    // Update sample index for both left and right partitions
    // Recursively grow the tree
    std::unique_ptr< RFNode > leftChild ( new RFNode() );
    std::unique_ptr< RFNode > rightChild ( new RFNode() );

    recursivePartition(
      leftChild.get(), &averagingLeftPartitionIndex,
      &splittingLeftPartitionIndex, trainingData, myseed
    );
    recursivePartition(
      rightChild.get(), &averagingRightPartitionIndex,
      &splittingRightPartitionIndex, trainingData, myseed
    );

    (*rootNode).setSplitNode(
      bestSplitFeature,
      bestSplitValue,
      std::move(leftChild),
      std::move(rightChild)
    );
  }
}

void honestRFTree::selectBestFeature(
  size_t &bestSplitFeature,
  double &bestSplitValue,
  double &bestSplitLoss,
  std::vector<size_t>* featureList,
  std::vector<size_t>* averagingSampleIndex,
  std::vector<size_t>* splittingSampleIndex,
  DataFrame* trainingData,
  unsigned int myseed
){
  // Get the number of total features
  size_t mtry = (*featureList).size();

  // Initialize the minimum loss for each feature
  double* bestSplitLossAll = new double[mtry];
  double* bestSplitValueAll = new double[mtry];
  size_t* bestSplitFeatureAll = new size_t[mtry];
  size_t* bestSplitCountAll = new size_t[mtry];

  for (size_t i=0; i<mtry; i++) {
    bestSplitLossAll[i] = -std::numeric_limits<double>::infinity();
    bestSplitValueAll[i] = std::numeric_limits<double>::quiet_NaN();
    bestSplitFeatureAll[i] = std::numeric_limits<size_t>::quiet_NaN();
    bestSplitCountAll[i] = 0;
  }

  // Iterate each selected features
  for (size_t i=0; i<mtry; i++) {
    size_t currentFeature = (*featureList)[i];
    // Test if the current feature is in the categorical list
    std::vector<size_t> categorialCols = *(*trainingData).getCatCols();
    if (
      std::find(
        categorialCols.begin(),
        categorialCols.end(),
        currentFeature
      ) != categorialCols.end()
    ){

      // Count total number of observations for different categories
      std::set<double> all_categories;
      double splitTotalSum = 0;
      size_t splitTotalCount = 0;
      size_t averageTotalCount = 0;

      for (size_t j=0; j<(*splittingSampleIndex).size(); j++) {
        all_categories.insert(
          (*trainingData).getPoint((*splittingSampleIndex)[j], currentFeature)
        );
        splitTotalSum +=
          (*trainingData).getOutcomePoint((*splittingSampleIndex)[j]);
        splitTotalCount++;
      }
      for (size_t j=0; j<(*averagingSampleIndex).size(); j++) {
        all_categories.insert(
          (*trainingData).getPoint((*averagingSampleIndex)[j], currentFeature)
        );
        averageTotalCount++;
      }

      // Create map to track the count and sum of y squares
      std::map<double, size_t> splittingCategoryCount;
      std::map<double, size_t> averagingCategoryCount;
      std::map<double, double> splittingCategoryYSum;

      for (
        std::set<double>::iterator it=all_categories.begin();
        it != all_categories.end();
        ++it
      ) {
        splittingCategoryCount[*it] = 0;
        averagingCategoryCount[*it] = 0;
        splittingCategoryYSum[*it] = 0;
      }

      for (size_t j=0; j<(*splittingSampleIndex).size(); j++) {
        double currentXValue = (*trainingData).
          getPoint((*splittingSampleIndex)[j], currentFeature);
        double currentYValue = (*trainingData).
          getOutcomePoint((*splittingSampleIndex)[j]);
        splittingCategoryCount[currentXValue] += 1;
        splittingCategoryYSum[currentXValue] += currentYValue;
      }

      for (size_t j=0; j<(*averagingSampleIndex).size(); j++) {
        double currentXValue = (*trainingData).
          getPoint((*averagingSampleIndex)[j], currentFeature);
        averagingCategoryCount[currentXValue] += 1;
      }

      // Go through the sums and determine the best partition
      for (
        std::set<double>::iterator it=all_categories.begin();
        it != all_categories.end();
        ++it
      ) {
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
        ) {
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
            double tmp_random = (double) rand_r(&myseed) / RAND_MAX;
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
    for (size_t j=0; j<(*splittingSampleIndex).size(); j++){
      // Retrieve the current feature value
      double tmpFeatureValue = (*trainingData).
        getPoint((*splittingSampleIndex)[j], currentFeature);
      double tmpOutcomeValue = (*trainingData).
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

    for (size_t j=0; j<(*averagingSampleIndex).size(); j++){
      // Retrieve the current feature value
      double tmpFeatureValue = (*trainingData).
        getPoint((*averagingSampleIndex)[j], currentFeature);
      double tmpOutcomeValue = (*trainingData).
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

    while (
      splittingDataIter < splittingData.end() ||
      averagingDataIter < averagingData.end()
    ){

      // Exhaust all current feature value in both dataset as partitioning
      while (
        splittingDataIter < splittingData.end() &&
        std::get<0>(*splittingDataIter) == featureValue
      ) {
        splitLeftPartitionCount++;
        splitLeftPartitionRunningSum += std::get<1>(*splittingDataIter);
        splittingDataIter++;
      }

      while (
        averagingDataIter < averagingData.end() &&
        std::get<0>(*averagingDataIter) == featureValue
      ) {
        averagingDataIter++;
        averageLeftPartitionCount++;
      }

      // Test if the all the values for the feature are the same, then proceed
      if (oneValueDistinctFlag) {
        oneValueDistinctFlag = false;
        if (
          splittingDataIter == splittingData.end() &&
          averagingDataIter == averagingData.end()
        ) {
          break;
        }
      }

      // Make partitions on the current feature and value in both splitting
      // and averaging dataset. `averageLeftPartitionCount` and
      // `splitLeftPartitionCount` already did the partition after we sort the
      // array.
      // Get new feature value
      if (
        splittingDataIter == splittingData.end() &&
        averagingDataIter == averagingData.end()
      ) {
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
      ) {
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
        double tmp_random = (double) rand_r(&myseed) / RAND_MAX;
        bestSplitValueAll[i] = tmp_random *
          (newFeatureValue - featureValue) + featureValue;
        bestSplitCountAll[i] = 1;

      } else {

        // If we are as good as the best split
        if (muBarSquareSum == bestSplitLossAll[i]) {

          bestSplitCountAll[i] = bestSplitCountAll[i] + 1;
          // Only update with probability 1/nseen
          double tmp_random = (double) rand_r(&myseed) / RAND_MAX;

          if (tmp_random * bestSplitCountAll[i] <= 1) {
            bestSplitLossAll[i] = muBarSquareSum;
            bestSplitFeatureAll[i] = currentFeature;
            tmp_random = (double) rand_r(&myseed) / RAND_MAX;
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

  for (size_t i=0; i<mtry; i++) {
    if (bestSplitLossAll[i] > bestSplitLoss_) {
      bestSplitLoss_ = bestSplitLossAll[i];
    }
  }


  for (size_t i=0; i<mtry; i++) {
    if (bestSplitLossAll[i] == bestSplitLoss_) {
      for (size_t j=0; j<bestSplitCountAll[i]; j++) {
        bestFeatures.push_back(i);
      }
    }
  }

  // If we found a feasible splitting point
  if (bestFeatures.size() > 0) {

    // If there are multiple best features, sample one according to their
    // frequency of occurence
    size_t tmp_random = rand_r(&myseed) % bestFeatures.size();
    size_t bestFeatureIndex = bestFeatures.at(tmp_random);
    // Return the best splitFeature and splitValue
    bestSplitFeature = bestSplitFeatureAll[bestFeatureIndex];
    bestSplitValue = bestSplitValueAll[bestFeatureIndex];
    bestSplitLoss = bestSplitLoss_;

  } else {

    // If none of the features are possible, return NA
    bestSplitFeature = std::numeric_limits<size_t>::quiet_NaN();
    bestSplitValue = std::numeric_limits<double>::quiet_NaN();
    bestSplitLoss = std::numeric_limits<double>::quiet_NaN();

  }

  delete[](bestSplitLossAll);
  delete[](bestSplitValueAll);
  delete[](bestSplitFeatureAll);
  delete[](bestSplitCountAll);

}

void honestRFTree::printTree(){
  (*getRoot()).printSubtree();
}

void honestRFTree::getOOBindex(
  std::vector<size_t> &outputOOBIndex,
  size_t nRows
){

  // Generate union of splitting and averaging dataset
  std::sort(
    (*getSplittingIndex()).begin(),
    (*getSplittingIndex()).end()
  );
  std::sort(
    (*getAveragingIndex()).begin(),
    (*getAveragingIndex()).end()
  );

  std::vector<size_t> allSampledIndex(
    (*getSplittingIndex()).size() + (*getAveragingIndex()).size()
  );

  std::vector<size_t>::iterator it= std::set_union(
    (*getSplittingIndex()).begin(),
    (*getSplittingIndex()).end(),
    (*getAveragingIndex()).begin(),
    (*getAveragingIndex()).end(),
    allSampledIndex.begin()
  );

  allSampledIndex.resize((unsigned long) (it - allSampledIndex.begin()));

  // Generate a vector of all index based on nRows
  struct IncGenerator {
    size_t current_;
    IncGenerator(size_t start): current_(start) {}
    size_t operator()() { return current_++; }
  };
  std::vector<size_t> allIndex(nRows);
  IncGenerator g(0);
  std::generate(allIndex.begin(), allIndex.end(), g);

  // OOB index is the set difference between sampled index and all index
  std::vector<size_t> OOBIndex(nRows);

  it = std::set_difference (
    allIndex.begin(),
    allIndex.end(),
    allSampledIndex.begin(),
    allSampledIndex.end(),
    OOBIndex.begin()
  );
  OOBIndex.resize((unsigned long) (it - OOBIndex.begin()));

  for (
    std::vector<size_t>::iterator it = OOBIndex.begin();
    it != OOBIndex.end();
    ++it
  ) {
    outputOOBIndex.push_back(*it);
  }

}

void honestRFTree::getOOBPrediction(
  std::vector<double> &outputOOBPrediction,
  std::vector<size_t> &outputOOBCount,
  DataFrame* trainingData
){

  std::vector<size_t> OOBIndex;
  getOOBindex(OOBIndex, trainingData->getNumRows());

  for (
    std::vector<size_t>::iterator it = OOBIndex.begin();
    it != OOBIndex.end();
    ++it
  ) {

    size_t OOBSampleIndex = *it;

    // Predict current oob sample
    std::vector<double> currentTreePrediction(1);
    std::vector<double> OOBSampleObservation((*trainingData).getNumColumns());
    (*trainingData).getObservationData(OOBSampleObservation, OOBSampleIndex);

    std::vector< std::vector<double> > OOBSampleObservation_;
    for (size_t k=0; k<(*trainingData).getNumColumns(); k++){
      std::vector<double> OOBSampleObservation_iter(1);
      OOBSampleObservation_iter[0] = OOBSampleObservation[k];
      OOBSampleObservation_.push_back(OOBSampleObservation_iter);
    }

    predict(
      currentTreePrediction,
      &OOBSampleObservation_,
      trainingData
    );

    // Update the global OOB vector
    outputOOBPrediction[OOBSampleIndex] += currentTreePrediction[0];
    outputOOBCount[OOBSampleIndex] += 1;
  }
}