// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include "DataFrame.h"
#include "honestRFTree.h"
#include "RFNode.h"
#include "honestRF.h"

void freeHonestRF(
  SEXP ptr
){
  if (NULL == R_ExternalPtrAddr(ptr))
    return;
  honestRF* pm = (honestRF*)(R_ExternalPtrAddr(ptr));
  delete(pm);
  R_ClearExternalPtr(ptr);
}

// [[Rcpp::export]]
SEXP rcpp_cppDataFrameInterface(
    Rcpp::List x,
    Rcpp::NumericVector y,
    Rcpp::NumericVector catCols,
    int numRows,
    int numColumns
){

  try {
    std::unique_ptr<std::vector< std::vector<double> > > featureDataRcpp (
        new std::vector< std::vector<double> >(
            Rcpp::as< std::vector< std::vector<double> > >(x)
        )
    );

    std::unique_ptr<std::vector<double>> outcomeDataRcpp (
        new std::vector<double>(
            Rcpp::as< std::vector<double> >(y)
        )
    );

    std::unique_ptr< std::vector<size_t> > categoricalFeatureColsRcpp (
        new std::vector<size_t>(
            Rcpp::as< std::vector<size_t> >(catCols)
        )
    );

    DataFrame* trainingData = new DataFrame(
        std::move(featureDataRcpp),
        std::move(outcomeDataRcpp),
        std::move(categoricalFeatureColsRcpp),
        (size_t) numRows,
        (size_t) numColumns
    );

    Rcpp::XPtr<DataFrame> ptr(trainingData, true) ;
    return ptr;

  } catch(std::runtime_error const& err) {
    forward_exception_to_r(err);
  } catch(...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  return NULL;
}


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
  int nthread,
  bool verbose,
  bool middleSplit,
  bool existing_dataframe_flag,
  SEXP existing_dataframe
){

  if (existing_dataframe_flag) {

    try {
      Rcpp::XPtr< DataFrame > trainingData(existing_dataframe) ;

      honestRF* testFullForest = new honestRF(
        trainingData,
        (size_t) ntree,
        replace,
        (size_t) sampsize,
        splitratio,
        (size_t) mtry,
        (size_t) nodesizeSpl,
        (size_t) nodesizeAvg,
        (unsigned int) seed,
        (size_t) nthread,
        verbose,
        middleSplit
      );

      // delete(testFullForest);
      Rcpp::XPtr<honestRF> ptr(testFullForest, true) ;
      R_RegisterCFinalizerEx(
        ptr,
        (R_CFinalizer_t) freeHonestRF,
        (Rboolean) TRUE
      );
      return ptr;
    } catch(std::runtime_error const& err) {
      forward_exception_to_r(err);
    } catch(...) {
      ::Rf_error("c++ exception (unknown reason)");
    }

  } else {

    try {
      std::unique_ptr<std::vector< std::vector<double> > > featureDataRcpp (
          new std::vector< std::vector<double> >(
              Rcpp::as< std::vector< std::vector<double> > >(x)
          )
      );

      std::unique_ptr<std::vector<double>> outcomeDataRcpp (
          new std::vector<double>(
              Rcpp::as< std::vector<double> >(y)
          )
      );

      std::unique_ptr< std::vector<size_t> > categoricalFeatureColsRcpp (
          new std::vector<size_t>(
              Rcpp::as< std::vector<size_t> >(catCols)
          )
      );

      DataFrame* trainingData = new DataFrame(
          std::move(featureDataRcpp),
          std::move(outcomeDataRcpp),
          std::move(categoricalFeatureColsRcpp),
          (size_t) numRows,
          (size_t) numColumns
      );

      honestRF* testFullForest = new honestRF(
        trainingData,
        (size_t) ntree,
        replace,
        (size_t) sampsize,
        splitratio,
        (size_t) mtry,
        (size_t) nodesizeSpl,
        (size_t) nodesizeAvg,
        (unsigned int) seed,
        (size_t) nthread,
        verbose,
        middleSplit
      );

      // delete(testFullForest);
      Rcpp::XPtr<honestRF> ptr(testFullForest, true) ;
      R_RegisterCFinalizerEx(
        ptr,
        (R_CFinalizer_t) freeHonestRF,
        (Rboolean) TRUE
      );
      return ptr;

    } catch(std::runtime_error const& err) {
      forward_exception_to_r(err);
    } catch(...) {
      ::Rf_error("c++ exception (unknown reason)");
    }
  }
  return NULL;
}

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_cppPredictInterface(
  SEXP forest,
  Rcpp::List x
){

  try {

    Rcpp::XPtr< honestRF > testFullForest(forest) ;

    std::vector< std::vector<double> > featureData =
      Rcpp::as< std::vector< std::vector<double> > >(x);

    std::unique_ptr< std::vector<double> > testForestPrediction (
      (*testFullForest).predict(&featureData)
    );

    std::vector<double>* testForestPrediction_ =
      new std::vector<double>(*testForestPrediction.get());

    Rcpp::NumericVector output = Rcpp::wrap(*testForestPrediction_);

    return output;

  } catch(std::runtime_error const& err) {
    forward_exception_to_r(err);
  } catch(...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  return NULL;
}


// [[Rcpp::export]]
double rcpp_OBBPredictInterface(
    SEXP forest
){

  try {
    Rcpp::XPtr< honestRF > testFullForest(forest) ;
    double OOBError = (*testFullForest).getOOBError();
    return OOBError;
  } catch(std::runtime_error const& err) {
    forward_exception_to_r(err);
  } catch(...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  return Rcpp::NumericVector::get_na() ;
}


// [[Rcpp::export]]
double rcpp_getObservationSizeInterface(
    SEXP df
){

  try {
    Rcpp::XPtr< DataFrame > trainingData(df) ;
    double nrows = (double) (*trainingData).getNumRows();
    return nrows;
  } catch(std::runtime_error const& err) {
    forward_exception_to_r(err);
  } catch(...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  return Rcpp::NumericVector::get_na() ;
}


// [[Rcpp::export]]
void rcpp_AddTreeInterface(
    SEXP forest,
    int ntree
){
  try {
    Rcpp::XPtr< honestRF > testFullForest(forest) ;
    (*testFullForest).addTrees(ntree);
  } catch(std::runtime_error const& err) {
    forward_exception_to_r(err);
  } catch(...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
}
