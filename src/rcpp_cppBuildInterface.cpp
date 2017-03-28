// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <vector>
#include "DataFrame.h"
#include "honestRFTree.h"
#include "RFNode.h"
#include "honestRF.h"

// [[Rcpp::export]]
SEXP rcpp_cppBuildInterface(
  Rcpp::List x,
  Rcpp::NumericVector y,
  Rcpp::NumericVector catCols,
  int numRows,
  int numColumns,
  int ntree,
  bool replace,
  int sampsize,
  int mtry,
  double splitratio,
  int nodesizeSpl,
  int nodesizeAvg,
  int seed,
  bool verbose){

  try {
    std::vector< std::vector<double> > featureData =
      Rcpp::as< std::vector< std::vector<double> > >(x);

    std::vector< std::vector<double> >* featureDataRcpp =
      new std::vector< std::vector<double> >(featureData);

    // std::vector<std::string> featureNames =
    //   Rcpp::as< std::vector<std::string> >(colNames);
    //
    // std::string outcomeName = (Rcpp::as< std::vector<std::string> >(yName))[0];

    std::vector<double> outcomeData = Rcpp::as< std::vector<double> >(y);
    std::vector<double>* outcomeDataRcpp =
      new std::vector<double>(outcomeData);

    std::vector<size_t> categoricalFeatureCols =
      Rcpp::as< std::vector<size_t> >(catCols);
    std::vector<size_t>* categoricalFeatureColsRcpp =
      new std::vector<size_t>(categoricalFeatureCols);

    DataFrame* trainingData = new DataFrame(
      featureDataRcpp,
      // &featureNames,
      outcomeDataRcpp,
      // outcomeName,
      categoricalFeatureColsRcpp,
      (size_t) numRows,
      (size_t) numColumns
    );

    honestRF *testFullForest = new honestRF(
      trainingData,
      (size_t) ntree,
      replace,
      (size_t) sampsize,
      splitratio,
      (size_t) mtry,
      (size_t) nodesizeSpl,
      (size_t) nodesizeAvg,
      (unsigned int) seed,
      verbose
    );

    Rcpp::XPtr<honestRF> p(testFullForest, true) ;

    return p;

  } catch (const char *msg) {
    std::cerr << msg << std::endl;

    return NULL;
  }

}

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_cppPredictInterface(
  SEXP forest,
  Rcpp::List x){

  try {
    Rcpp::XPtr<honestRF> testFullForest(forest) ;

    std::vector< std::vector<double> > featureData =
      Rcpp::as< std::vector< std::vector<double> > >(x);

    // // Print first two trees
    // std::vector<honestRFTree>* firstForest = (*testFullForest).getForest();
    // (*firstForest)[0].printTree();

    std::vector<double>* testForestPrediction =
      (*testFullForest).predict(&featureData);

    Rcpp::NumericVector output = Rcpp::wrap(*testForestPrediction);

    return output;

  } catch (const char *msg) {
    std::cerr << msg << std::endl;

    return NULL;
  }
}
