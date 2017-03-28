// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_cppBuildInterface
SEXP rcpp_cppBuildInterface(Rcpp::List x, Rcpp::NumericVector y, Rcpp::NumericVector catCols, int numRows, int numColumns, int ntree, bool replace, int sampsize, int mtry, double splitratio, int nodesizeSpl, int nodesizeAvg, int seed);
RcppExport SEXP hte_rcpp_cppBuildInterface(SEXP xSEXP, SEXP ySEXP, SEXP catColsSEXP, SEXP numRowsSEXP, SEXP numColumnsSEXP, SEXP ntreeSEXP, SEXP replaceSEXP, SEXP sampsizeSEXP, SEXP mtrySEXP, SEXP splitratioSEXP, SEXP nodesizeSplSEXP, SEXP nodesizeAvgSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type catCols(catColsSEXP);
    Rcpp::traits::input_parameter< int >::type numRows(numRowsSEXP);
    Rcpp::traits::input_parameter< int >::type numColumns(numColumnsSEXP);
    Rcpp::traits::input_parameter< int >::type ntree(ntreeSEXP);
    Rcpp::traits::input_parameter< bool >::type replace(replaceSEXP);
    Rcpp::traits::input_parameter< int >::type sampsize(sampsizeSEXP);
    Rcpp::traits::input_parameter< int >::type mtry(mtrySEXP);
    Rcpp::traits::input_parameter< double >::type splitratio(splitratioSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeSpl(nodesizeSplSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeAvg(nodesizeAvgSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_cppBuildInterface(x, y, catCols, numRows, numColumns, ntree, replace, sampsize, mtry, splitratio, nodesizeSpl, nodesizeAvg, seed));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_cppPredictInterface
Rcpp::NumericVector rcpp_cppPredictInterface(SEXP forest, Rcpp::List x);
RcppExport SEXP hte_rcpp_cppPredictInterface(SEXP forestSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type forest(forestSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_cppPredictInterface(forest, x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_selectBestFeature
List rcpp_selectBestFeature(DataFrame x, NumericVector y, List featureList, List sampleIndex, List nodesize, std::string splitrule, List categoricalFeatureCols);
RcppExport SEXP hte_rcpp_selectBestFeature(SEXP xSEXP, SEXP ySEXP, SEXP featureListSEXP, SEXP sampleIndexSEXP, SEXP nodesizeSEXP, SEXP splitruleSEXP, SEXP categoricalFeatureColsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< List >::type featureList(featureListSEXP);
    Rcpp::traits::input_parameter< List >::type sampleIndex(sampleIndexSEXP);
    Rcpp::traits::input_parameter< List >::type nodesize(nodesizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type splitrule(splitruleSEXP);
    Rcpp::traits::input_parameter< List >::type categoricalFeatureCols(categoricalFeatureColsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_selectBestFeature(x, y, featureList, sampleIndex, nodesize, splitrule, categoricalFeatureCols));
    return rcpp_result_gen;
END_RCPP
}
