// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <limits.h>
#include <tuple>
#include <vector>

using namespace Rcpp;

// Return a R vector with two variables, best_pivot, and min_loss.
// The function implements the nlogn algorithm of computing the
// in-group variance for every possible split point in C++. The
// input parameter is a R dataframe object. Please note that the
// dataframe is presorted according to variable x!
// [[Rcpp::export]]
List rcpp_selectBestFeature(
    DataFrame x,
    NumericVector y,
    List featureList,
    List sampleIndex,
    List nodesize,
    std::string splitrule
) {

  // Get the number of total features
  int mtry = featureList.size();

  // Initialize the minimum loss for each feature
  double *bestSplitLossAll = new double[mtry];
  double *bestSplitValueAll = new double[mtry];
  double *bestSplitFeatureAll = new double[mtry];
  double *bestSplitCountAll = new double[mtry];

  for (int i=0; i<mtry; i++) {
    bestSplitLossAll[i] = -std::numeric_limits<double>::infinity();
    bestSplitValueAll[i] = NumericVector::get_na();
    bestSplitFeatureAll[i] = NumericVector::get_na();
    bestSplitCountAll[i] = 0;
  }

  // Iterate each selected features
  for (int i=0; i<mtry; i++) {
    // Note: R index
    int currentFeature = featureList[i];
    NumericVector currentFeatureValues = x[currentFeature-1];

    // Extract sample index
    NumericVector splittingSampleIndex = as<NumericVector>(
      sampleIndex["splittingSampleIndex"]
    );
    NumericVector averagingSampleIndex = as<NumericVector>(
      sampleIndex["averagingSampleIndex"]
    );

    int splittingNodeSize = as<int>(nodesize["splittingNodeSize"]);
    int averagingNodeSize = as<int>(nodesize["averagingNodeSize"]);

    // Create specific vectors to holddata
    typedef std::tuple<double,double> dataPair;
    std::vector<dataPair> splittingData;
    std::vector<dataPair> averagingData;
    double splitTotalSum = 0;
    for (int j=0; j<splittingSampleIndex.size(); j++){
      // Retrieve the current feature value
      double tmpFeatureValue = currentFeatureValues[splittingSampleIndex[j]-1];
      splitTotalSum += y[splittingSampleIndex[j]-1];

      // Adding data to the internal data vector (Note: R index)
      splittingData.push_back(
        std::make_tuple(
          tmpFeatureValue,
          y[splittingSampleIndex[j]-1]
        )
      );
    }

    for (int j=0; j<averagingSampleIndex.size(); j++){
      // Retrieve the current feature value
      double tmpFeatureValue = currentFeatureValues[averagingSampleIndex[j]-1];

      // Adding data to the internal data vector
      averagingData.push_back(
        std::make_tuple(
          tmpFeatureValue,
          y[averagingSampleIndex[j]-1]
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

    int splitLeftPartitionCount = 0;
    int averageLeftPartitionCount = 0;

    int splitTotalCount = splittingData.size();
    int averageTotalCount = averagingData.size();

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
        splittingDataIter < splittingData.end() |
          averagingDataIter < averagingData.end()
    ){

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
          ) < splittingNodeSize||
            std::min(
              averageLeftPartitionCount,
              averageTotalCount - averageLeftPartitionCount
            ) < averagingNodeSize
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
            bestSplitValueAll[i] = tmp_random *
              (newFeatureValue - featureValue) + featureValue;
          }
        }
      }

      // Update the old feature value
      featureValue = newFeatureValue;
    }
  }


  // Get the best split values among all features
  double bestSplitLoss = -std::numeric_limits<double>::infinity();
  std::vector<int> bestFeatures;

  for (int i=0; i<mtry; i++) {
    if (bestSplitLossAll[i] > bestSplitLoss) {
      bestSplitLoss = bestSplitLossAll[i];
    }
  }

  for (int i=0; i<mtry; i++) {
    if (bestSplitLossAll[i] == bestSplitLoss) {
      for (int j=0; j<bestSplitCountAll[i]; j++){
        bestFeatures.push_back(i);
      }
    }
  }

  // If we found a feasible splitting point
  if (bestFeatures.size() > 0) {
    // If there are multiple best features, sample one according to their
    // frequency of occurence
    int tmp_random = rand() % bestFeatures.size();
    int bestFeatureIndex = bestFeatures.at(tmp_random);
    // Return the best splitFeature and splitValue
    return List::create(
      Named("bestSplitFeature") = bestSplitFeatureAll[bestFeatureIndex],
      Named("bestSplitValue") = bestSplitValueAll[bestFeatureIndex]
    );
  }else{
    // If none of the features are possible, return NA
    return List::create(
      Named("bestSplitFeature") = NumericVector::get_na(),
      Named("bestSplitValue") = NumericVector::get_na()
    );
  }
}
