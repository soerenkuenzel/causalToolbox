#include <algorithm>
#include "DataFrame.h"

DataFrame::DataFrame():
  featureData(0),
//  featureNames(0),
  outcomeData(0),
//  outcomeName(0),
  categoricalFeatureCols(0), numRows(0), numColumns(0) {}

DataFrame::~DataFrame(){}

DataFrame::DataFrame(
  std::vector< std::vector<double> >* featureData,
//  std::vector<std::string>* featureNames,
  std::vector<double>* outcomeData,
//  std::string outcomeName,
  std::vector<size_t>* categoricalFeatureCols,
  std::size_t numRows,
  std::size_t numColumns
) {
  this->featureData = featureData;
//  this->featureNames = featureNames;
  this->outcomeData = outcomeData;
//  this->outcomeName = outcomeName;
  this->categoricalFeatureCols = categoricalFeatureCols;
  this->numRows = numRows;
  this->numColumns = numColumns;
}

double DataFrame::getPoint(size_t rowIndex, size_t colIndex){
  // Check if rowIndex and colIndex are valid
  if (rowIndex < numRows && colIndex < numColumns) {
    return (*featureData)[colIndex][rowIndex];
  } else {
    throw "Invalid rowIndex or colIndex.";
  }
}

double DataFrame::getOutcomePoint(size_t rowIndex){
  // Check if rowIndex is valid
  if (rowIndex < numRows) {
    return (*outcomeData)[rowIndex];
  } else {
    throw "Invalid rowIndex.";
  }
}

//std::string DataFrame::getFeatureName(size_t colIndex){
//  if (colIndex < numColumns) {
//    return (*featureNames)[colIndex];
//  } else {
//    throw "Invalid colIndex.";
//  }
//}
//
//size_t DataFrame::getFeatureIndex(std::string colName){
//  std::vector<std::string>::iterator searchColNam= std::find(
//    (*featureNames).begin(),
//    (*featureNames).end(),
//    colName
//  );
//  if (searchColNam != (*featureNames).end()) {
//    // Found
//    ptrdiff_t colIndex = searchColNam - (*featureNames).begin();
//    return (size_t) colIndex;
//  } else {
//    throw "Index name not found.";
//  }
//}

std::vector<double>* DataFrame::getFeatureData(size_t colIndex){
  if (colIndex < numColumns) {
    return &(*featureData)[colIndex];
  } else {
    throw "Invalid colIndex.";
  }
}

void DataFrame::getObservationData(
  std::vector<double> &rowData,
  size_t rowIndex
){
  if (rowIndex < numRows){
    for (size_t i=0; i < numColumns; i++){
      rowData[i] = (*featureData)[i][rowIndex];
    }
  } else {
    throw "Invalid rowIndex.";
  }
}

double DataFrame::partitionMean(std::vector<size_t>* sampleIndex){
  size_t totalSampleSize = (*sampleIndex).size();
  double accummulatedSum = 0;
  for(std::vector<size_t>::iterator it = (*sampleIndex).begin();
      it != (*sampleIndex).end();
      ++it) {
    accummulatedSum += getOutcomePoint(*it);
  }
  return accummulatedSum/totalSampleSize;
}

