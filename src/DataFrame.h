#ifndef HTECPP_DATAFRAME_H
#define HTECPP_DATAFRAME_H

#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <memory>

class DataFrame {

public:
  DataFrame();
  virtual ~DataFrame();

  DataFrame(
    std::unique_ptr< std::vector< std::vector<double> > > featureData,
    std::unique_ptr< std::vector<double> > outcomeData,
    std::unique_ptr< std::vector<size_t> > categoricalFeatureCols,
    std::size_t numRows,
    std::size_t numColumns
  );

  double getPoint(size_t rowIndex, size_t colIndex);

  double getOutcomePoint(size_t rowIndex);

  std::vector<double>* getFeatureData(size_t colIndex);

  void getObservationData(std::vector<double> &rowData, size_t rowIndex);

  double partitionMean(std::vector<size_t>* sampleIndex);

  std::vector< std::vector<double> >* getAllFeatureData() {
    return _featureData.get();
  }

  std::vector<double>* getOutcomeData() {
    return _outcomeData.get();
  }

  size_t getNumColumns() {
    return _numColumns;
  }

  size_t getNumRows() {
    return _numRows;
  }

  std::vector<size_t>* getCatCols() {
    return _categoricalFeatureCols.get();
  }

private:
  std::unique_ptr< std::vector< std::vector<double> > > _featureData;
  std::unique_ptr< std::vector<double> > _outcomeData;
  std::unique_ptr< std::vector<size_t> > _categoricalFeatureCols;
  std::size_t _numRows;
  std::size_t _numColumns;
};


#endif //HTECPP_DATAFRAME_H
