#include <iostream>
#include <vector>
#include <string>
#include <cmath>
#include <algorithm>

#include "DataFrame.h"
#include "RFNode.h"
#include "honestRFTree.h"
#include "honestRF.h"

#define TEST_DATAFRAME false
#define TEST_RFNode false
#define TEST_RFTree false
#define TEST_RF true

int main() {
  std::cout << "Test starts" << std::endl;

  std::vector<std::string> featureNames = {
    "Sepal.Length",
    "Sepal.Width",
    "Petal.Length",
    "Species"
  };

  std::string outcomeName = "Petal.Width";

  size_t numRows = 150;
  size_t numColumns = featureNames.size();

  std::vector< std::vector<double> > featureData = {
    {
      5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4, 4.9,
      5.4, 4.8, 4.8, 4.3, 5.8, 5.7, 5.4, 5.1, 5.7, 5.1,
      5.4, 5.1, 4.6, 5.1, 4.8, 5.0, 5.0, 5.2, 5.2, 4.7,
      4.8, 5.4, 5.2, 5.5, 4.9, 5.0, 5.5, 4.9, 4.4, 5.1,
      5.0, 4.5, 4.4, 5.0, 5.1, 4.8, 5.1, 4.6, 5.3, 5.0,
      7.0, 6.4, 6.9, 5.5, 6.5, 5.7, 6.3, 4.9, 6.6, 5.2,
      5.0, 5.9, 6.0, 6.1, 5.6, 6.7, 5.6, 5.8, 6.2, 5.6,
      5.9, 6.1, 6.3, 6.1, 6.4, 6.6, 6.8, 6.7, 6.0, 5.7,
      5.5, 5.5, 5.8, 6.0, 5.4, 6.0, 6.7, 6.3, 5.6, 5.5,
      5.5, 6.1, 5.8, 5.0, 5.6, 5.7, 5.7, 6.2, 5.1, 5.7,
      6.3, 5.8, 7.1, 6.3, 6.5, 7.6, 4.9, 7.3, 6.7, 7.2,
      6.5, 6.4, 6.8, 5.7, 5.8, 6.4, 6.5, 7.7, 7.7, 6.0,
      6.9, 5.6, 7.7, 6.3, 6.7, 7.2, 6.2, 6.1, 6.4, 7.2,
      7.4, 7.9, 6.4, 6.3, 6.1, 7.7, 6.3, 6.4, 6.0, 6.9,
      6.7, 6.9, 5.8, 6.8, 6.7, 6.7, 6.3, 6.5, 6.2, 5.9
    },
    {
      3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, 3.1,
      3.7, 3.4, 3.0, 3.0, 4.0, 4.4, 3.9, 3.5, 3.8, 3.8,
      3.4, 3.7, 3.6, 3.3, 3.4, 3.0, 3.4, 3.5, 3.4, 3.2,
      3.1, 3.4, 4.1, 4.2, 3.1, 3.2, 3.5, 3.6, 3.0, 3.4,
      3.5, 2.3, 3.2, 3.5, 3.8, 3.0, 3.8, 3.2, 3.7, 3.3,
      3.2, 3.2, 3.1, 2.3, 2.8, 2.8, 3.3, 2.4, 2.9, 2.7,
      2.0, 3.0, 2.2, 2.9, 2.9, 3.1, 3.0, 2.7, 2.2, 2.5,
      3.2, 2.8, 2.5, 2.8, 2.9, 3.0, 2.8, 3.0, 2.9, 2.6,
      2.4, 2.4, 2.7, 2.7, 3.0, 3.4, 3.1, 2.3, 3.0, 2.5,
      2.6, 3.0, 2.6, 2.3, 2.7, 3.0, 2.9, 2.9, 2.5, 2.8,
      3.3, 2.7, 3.0, 2.9, 3.0, 3.0, 2.5, 2.9, 2.5, 3.6,
      3.2, 2.7, 3.0, 2.5, 2.8, 3.2, 3.0, 3.8, 2.6, 2.2,
      3.2, 2.8, 2.8, 2.7, 3.3, 3.2, 2.8, 3.0, 2.8, 3.0,
      2.8, 3.8, 2.8, 2.8, 2.6, 3.0, 3.4, 3.1, 3.0, 3.1,
      3.1, 3.1, 2.7, 3.2, 3.3, 3.0, 2.5, 3.0, 3.4, 3.0
    },
    {
      1.4, 1.4, 1.3, 1.5, 1.4, 1.7, 1.4, 1.5, 1.4, 1.5,
      1.5, 1.6, 1.4, 1.1, 1.2, 1.5, 1.3, 1.4, 1.7, 1.5,
      1.7, 1.5, 1.0, 1.7, 1.9, 1.6, 1.6, 1.5, 1.4, 1.6,
      1.6, 1.5, 1.5, 1.4, 1.5, 1.2, 1.3, 1.4, 1.3, 1.5,
      1.3, 1.3, 1.3, 1.6, 1.9, 1.4, 1.6, 1.4, 1.5, 1.4,
      4.7, 4.5, 4.9, 4.0, 4.6, 4.5, 4.7, 3.3, 4.6, 3.9,
      3.5, 4.2, 4.0, 4.7, 3.6, 4.4, 4.5, 4.1, 4.5, 3.9,
      4.8, 4.0, 4.9, 4.7, 4.3, 4.4, 4.8, 5.0, 4.5, 3.5,
      3.8, 3.7, 3.9, 5.1, 4.5, 4.5, 4.7, 4.4, 4.1, 4.0,
      4.4, 4.6, 4.0, 3.3, 4.2, 4.2, 4.2, 4.3, 3.0, 4.1,
      6.0, 5.1, 5.9, 5.6, 5.8, 6.6, 4.5, 6.3, 5.8, 6.1,
      5.1, 5.3, 5.5, 5.0, 5.1, 5.3, 5.5, 6.7, 6.9, 5.0,
      5.7, 4.9, 6.7, 4.9, 5.7, 6.0, 4.8, 4.9, 5.6, 5.8,
      6.1, 6.4, 5.6, 5.1, 5.6, 6.1, 5.6, 5.5, 4.8, 5.4,
      5.6, 5.1, 5.1, 5.9, 5.7, 5.2, 5.0, 5.2, 5.4, 5.1
    },
    {
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3
    }
  };

  std::vector<double> outcomeData = {
    0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.3, 0.2, 0.2, 0.1,
    0.2, 0.2, 0.1, 0.1, 0.2, 0.4, 0.4, 0.3, 0.3, 0.3,
    0.2, 0.4, 0.2, 0.5, 0.2, 0.2, 0.4, 0.2, 0.2, 0.2,
    0.2, 0.4, 0.1, 0.2, 0.2, 0.2, 0.2, 0.1, 0.2, 0.2,
    0.3, 0.3, 0.2, 0.6, 0.4, 0.3, 0.2, 0.2, 0.2, 0.2,
    1.4, 1.5, 1.5, 1.3, 1.5, 1.3, 1.6, 1.0, 1.3, 1.4,
    1.0, 1.5, 1.0, 1.4, 1.3, 1.4, 1.5, 1.0, 1.5, 1.1,
    1.8, 1.3, 1.5, 1.2, 1.3, 1.4, 1.4, 1.7, 1.5, 1.0,
    1.1, 1.0, 1.2, 1.6, 1.5, 1.6, 1.5, 1.3, 1.3, 1.3,
    1.2, 1.4, 1.2, 1.0, 1.3, 1.2, 1.3, 1.3, 1.1, 1.3,
    2.5, 1.9, 2.1, 1.8, 2.2, 2.1, 1.7, 1.8, 1.8, 2.5,
    2.0, 1.9, 2.1, 2.0, 2.4, 2.3, 1.8, 2.2, 2.3, 1.5,
    2.3, 2.0, 2.0, 1.8, 2.1, 1.8, 1.8, 1.8, 2.1, 1.6,
    1.9, 2.0, 2.2, 1.5, 1.4, 2.3, 2.4, 1.8, 1.8, 2.1,
    2.4, 2.3, 1.9, 2.3, 2.5, 2.3, 1.9, 2.0, 2.3, 1.8
  };

  std::vector<size_t> categoricalFeatureCols = {
    3
  };

  DataFrame iris = DataFrame(
    &featureData,
//    &featureNames,
    &outcomeData,
//    outcomeName,
    &categoricalFeatureCols,
    numRows,
    numColumns
  );

  // Test Prep
  struct IncGenerator {
    size_t current_;
    IncGenerator(size_t start): current_(start) {}
    size_t operator()(){return current_++;}
  };
  std::vector<size_t> testAllSampleIndex(numRows) ;
  IncGenerator g(0);
  std::generate(testAllSampleIndex.begin(), testAllSampleIndex.end(), g);

  // Test for DataFrame Object
  if (TEST_DATAFRAME) {
    try {
      // Test getPoint, expected 1.7
      std::cout << iris.getPoint(5, 2) << std::endl;

      // Test getOutcomePoint, expected 0.4
      std::cout << iris.getOutcomePoint(5) << std::endl;

      // Test getFeatureName, expected Petal.Length
//      std::cout << iris.getFeatureName(2) << std::endl;

      // Test getFeatureIndex, expected 2
//      std::cout << iris.getFeatureIndex("Petal.Length") << std::endl;

      // Test getFeatureData
      std::vector<double> testFeatureColumn = *iris.getFeatureData(2);
      for (auto i = testFeatureColumn.begin();
           i != testFeatureColumn.end(); ++i) {
        std::cout << *i << ' ';
      }
      std::cout << std::endl;

      // Test getFeatureNames
//      std::vector<std::string> testFeatureNames = *iris.getFeatureNames();
//      for (auto i = testFeatureNames.begin();
//           i != testFeatureNames.end(); ++i) {
//        std::cout << *i << ' ';
//      }
//      std::cout << std::endl;

      // Test getObservationData
      std::vector<double> testSampleRow(numColumns);
      iris.getObservationData(testSampleRow, 4);
      for (auto i = testSampleRow.begin(); i != testSampleRow.end(); ++i) {
        std::cout << *i << ' ';
      }
      std::cout << std::endl;

      // Test getOutcomeData
      std::vector<double> testOutcome = *iris.getOutcomeData();
      for (auto i = testOutcome.begin(); i != testOutcome.end(); ++i) {
        std::cout << *i << ' ';
      }
      std::cout << std::endl;

    } catch (const char *msg) {
      std::cerr << msg << std::endl;
    }
  }

  if (TEST_RFNode) {
    try {
      size_t testSplitFeature = 1;
      double testSplitValue = 3.1;
      std::vector<size_t> leftPartitionIndex;
      std::vector<size_t> rightPartitionIndex;
      for(std::vector<size_t>::iterator it = testAllSampleIndex.begin();
          it != testAllSampleIndex.end();
          ++it) {
        if (featureData[testSplitFeature][*it] < testSplitValue) {
          leftPartitionIndex.push_back(*it);
        } else {
          rightPartitionIndex.push_back(*it);
        }
      }

      // Test RFNode constructor for leaf node
      RFNode leafNode1 = RFNode(
        &leftPartitionIndex,
        &leftPartitionIndex
      );
      RFNode leafNode2 = RFNode(
        &rightPartitionIndex,
        &rightPartitionIndex
      );

      std::cout << leafNode1.is_leaf() << std::endl;
      std::cout << leafNode1.getSplitCount() << std::endl;

      leafNode1.printSubtree();
      leafNode2.printSubtree();

      std::vector<double> leafPrediction(numRows);
      leafNode1.predict(leafPrediction, &leftPartitionIndex, &featureData, &iris);
      leafNode2.predict(leafPrediction, &rightPartitionIndex, &featureData, &iris);
      for (auto i = leafPrediction.begin(); i != leafPrediction.end(); ++i) {
        std::cout << *i << ' ';
      }
      std::cout << std::endl;

      // Test RFNode constructor for tree node
      RFNode splitNode = RFNode(1, 3.1, &leafNode1, &leafNode2);

      std::cout << splitNode.is_leaf() << std::endl;
      std::cout << splitNode.getSplitValue() << std::endl;
      std::cout << splitNode.getSplitFeature() << std::endl;

      splitNode.printSubtree();

      std::vector<double> splitPrediction(numRows);
      splitNode.predict(splitPrediction, &testAllSampleIndex, &featureData, &iris);
      for (auto i = splitPrediction.begin(); i != splitPrediction.end(); ++i) {
        std::cout << *i << ' ';
      }
      std::cout << std::endl;

    } catch (const char *msg) {
      std::cerr << msg << std::endl;
    }
  }

  if (TEST_RFTree) {
    try {
      honestRFTree testDummyTree = honestRFTree();
      testDummyTree.setDummyTree(&iris, 4, 5, 5,
                                 &testAllSampleIndex, &testAllSampleIndex);
      double testBestSplitValue;
      size_t testBestSplitFeature;
      double testBestSplitLoss;
      std::vector<size_t> testFeatureList = {0, 3};
      testDummyTree.selectBestFeature(
        testBestSplitFeature,
        testBestSplitValue,
        testBestSplitLoss,
        &testFeatureList,
        &testAllSampleIndex,
        &testAllSampleIndex,
        24750371
      );
      // Expected 3
      std::cout << testBestSplitFeature << ' ';
      // Expected 1
      std::cout << testBestSplitValue << ' ';
      std::cout << std::endl;

      testFeatureList = {0, 1};
      testDummyTree.selectBestFeature(
        testBestSplitFeature,
        testBestSplitValue,
        testBestSplitLoss,
        &testFeatureList,
        &testAllSampleIndex,
        &testAllSampleIndex,
        24750371
      );
      // Expected 0
      std::cout << testBestSplitFeature << ' ';
      // Expected 5.5589
      std::cout << testBestSplitValue << ' ';
      std::cout << std::endl;

      // Test full tree
      srand (24750371);
      honestRFTree testFullTree = honestRFTree(
        &iris, 4, 5, 5,
        &testAllSampleIndex, &testAllSampleIndex,
        24750371
      );
      // Test showtree
      testFullTree.printTree();

      // Test predict
      std::vector<double> testTreePrediction(numRows);
      testFullTree.predict(testTreePrediction, &featureData);
      for (auto i=testTreePrediction.begin();
           i!=testTreePrediction.end();
           ++i) {
        std::cout << *i << ' ';
      }
      std::cout << std::endl;

      // Calculate prediction accuracy
      double testMSE = 0;
      for (size_t i=0; i<outcomeData.size(); i++){
        testMSE += pow((testTreePrediction[i] - outcomeData[i]), 2);
      }
      std::cout << testMSE << std::endl;

    } catch (const char *msg) {
      std::cerr << msg << std::endl;
    }
  }

  if (TEST_RF) {
    try {
      // Test RF
      honestRF testFullForest = honestRF(
        &iris, 500, true, 150, 1, 3, 5, 5, 24750371, true
      );
      // Print first two trees
      std::vector<honestRFTree>* forest = testFullForest.getForest();
      (*forest)[0].printTree();
      (*forest)[1].printTree();
      // Test prediction
      std::vector<double>* testForestPrediction =
              testFullForest.predict(&featureData);
      // Calculate prediction accuracy
      double testMSE = 0;
      for (size_t i=0; i<outcomeData.size(); i++){
        testMSE += pow(((*testForestPrediction)[i] - outcomeData[i]), 2);
      }
      std::cout << testMSE << std::endl;

    } catch (const char *msg) {
      std::cerr << msg << std::endl;
    }
  }

  return 0;
}