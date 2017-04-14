#include "DataFrame.h"

DataFrame::DataFrame():
  _featureData(nullptr), _outcomeData(nullptr),
  _categoricalFeatureCols(nullptr), _numRows(0), _numColumns(0) {}

DataFrame::~DataFrame() {
//  std::cout << "DataFrame() destructor is called." << std::endl;
}

DataFrame::DataFrame(
  std::unique_ptr< std::vector< std::vector<double> > > featureData,
  std::unique_ptr< std::vector<double> > outcomeData,
  std::unique_ptr< std::vector<size_t> > categoricalFeatureCols,
  std::size_t numRows,
  std::size_t numColumns
) {
  this->_featureData = std::move(featureData);
  this->_outcomeData = std::move(outcomeData);
  this->_categoricalFeatureCols = std::move(categoricalFeatureCols);
  this->_numRows = numRows;
  this->_numColumns = numColumns;
}

double DataFrame::getPoint(size_t rowIndex, size_t colIndex) {
  // Check if rowIndex and colIndex are valid
  if (rowIndex < getNumRows() && colIndex < getNumColumns()) {
    return (*getAllFeatureData())[colIndex][rowIndex];
  } else {
    throw std::runtime_error("Invalid rowIndex or colIndex.");
  }
}

double DataFrame::getOutcomePoint(size_t rowIndex) {
  // Check if rowIndex is valid
  if (rowIndex < getNumRows()) {
    return (*getOutcomeData())[rowIndex];
  } else {
    throw std::runtime_error("Invalid rowIndex.");
  }
}

std::vector<double>* DataFrame::getFeatureData(
  size_t colIndex
) {
  if (colIndex < getNumColumns()) {
    return &(*getAllFeatureData())[colIndex];
  } else {
    throw std::runtime_error("Invalid colIndex.");
  }
}

void DataFrame::getObservationData(
  std::vector<double> &rowData,
  size_t rowIndex
) {
  if (rowIndex < getNumRows()) {
    for (size_t i=0; i < getNumColumns(); i++) {
      rowData[i] = (*getAllFeatureData())[i][rowIndex];
    }
  } else {
    throw std::runtime_error("Invalid rowIndex.");
  }
}

double DataFrame::partitionMean(
  std::vector<size_t>* sampleIndex
){
  size_t totalSampleSize = (*sampleIndex).size();
  double accummulatedSum = 0;
  for (
    std::vector<size_t>::iterator it = (*sampleIndex).begin();
    it != (*sampleIndex).end();
    ++it
  ) {
    accummulatedSum += getOutcomePoint(*it);
  }
  return accummulatedSum / totalSampleSize;
}
