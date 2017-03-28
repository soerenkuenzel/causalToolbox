#ifndef HTECPP_DATAFRAME_H
#define HTECPP_DATAFRAME_H

#include <iostream>
#include <vector>
#include <string>

class DataFrame {
public:
  DataFrame();
  virtual ~DataFrame();

  DataFrame(
    std::vector< std::vector<double> >* featureData,
//    std::vector<std::string>* featureNames,
    std::vector<double>* outcomeData,
//    std::string outcomeName,
    std::vector<size_t>* categoricalFeatureCols,
    std::size_t numRows,
    std::size_t numColumns
  );

  double getPoint(size_t rowIndex, size_t colIndex);

  double getOutcomePoint(size_t rowIndex);

//  std::string getFeatureName(size_t colIndex);
//
//  size_t getFeatureIndex(std::string colName);

  std::vector<double>* getFeatureData(size_t colIndex);

  void getObservationData(std::vector<double> &rowData, size_t rowIndex);

  double partitionMean(std::vector<size_t>* sampleIndex);

//  std::vector<std::string>* getFeatureNames(){
//    return featureNames;
//  }

  std::vector<double>* getOutcomeData(){
    return outcomeData;
  }

  size_t getNumColumns(){
    return numColumns;
  }

  size_t getNumRows(){
    return numRows;
  }

  std::vector<size_t>* getCatCols(){
    return categoricalFeatureCols;
  }

private:
  std::vector< std::vector<double> >* featureData;
//  std::vector<std::string>* featureNames;
  std::vector<double>* outcomeData;
//  std::string outcomeName;
  std::vector<size_t>* categoricalFeatureCols;
  std::size_t numRows;
  std::size_t numColumns;
};


#endif //HTECPP_DATAFRAME_H
