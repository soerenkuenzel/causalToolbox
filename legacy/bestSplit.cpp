#include <Rcpp.h>
#include <limits.h>
using namespace Rcpp;

// Return a R vector with two variables, best_pivot, and min_loss.
// The function implements the nlogn algorithm of computing the 
// in-group variance for every possible split point in C++. The 
// input parameter is a R dataframe object. Please note that the
// dataframe is presorted according to variable x!
// [[Rcpp::export]]
List computeBestSplit(NumericVector x, NumericVector y) {
  
  // Save the feature and outcome variables separately.
  // NumericVector x = df["X"];
  // NumericVector y = df["Y"];
  
  // Count total number of observations.
  int nrow = x.size();
  
  // Iterate through the list of vector X to calculate
  // the total sum of Y, Y^2 before the current observation. 
  double *A = new double[nrow]; // Track total sum of Y
  int *B = new int[nrow]; // Track total count 
  double *C = new double[nrow]; // Track total sum of Y^2
  
  // Set up the global counter for the total sum of Y, and Y^2.
  double S = 0; // Total sum of Y
  double W = 0; // Total sum of Y^2
  
  // Base case, all three arrays are set to 0.
  if (nrow > 0) {
    A[0] = 0;
    B[0] = 0;
    C[0] = 0;
    S = y[0];
    W = y[0] * y[0];
  }
  
  // Iterate through the vector to calculate the accumulative sum
  // of Y, Y^2, and the count of observations that satisfy X[i] < pivot  
  int same_count = 1;
  for (int i=1; i<nrow; i++) {
    // If we see a larger value, we increase the total sum.
    // Otherwise, we maintain the same values as before.
    // same_count is used to mitigate the issue of duplicate values.
    if (x[i] > x[i-1]) {
      A[i] = A[i-1] + y[i-1] * same_count;
      B[i] = B[i-1] + 1 * same_count;
      C[i] = C[i-1] + y[i-1] * y[i-1] * same_count;
      same_count = 1;
    } else {
      A[i] = A[i-1];
      B[i] = B[i-1];
      C[i] = C[i-1];
      same_count = same_count + 1;
    }
    
    // Increase the global total sum counter
    W = W + y[i] * y[i];
    S = S + y[i];
  }
  
  // Iterate through the vector again and calculate the variance of the 
  // split. In the meantime, we keep track the best split and minimum
  // variance.
  double best_pivot = NumericVector::get_na();
  double min_loss = std::numeric_limits<double>::infinity();
  for (int i=0; i<nrow; i++) {
    // Calculate in-group mean
    double e1;
    if (B[i] == 0) {
      e1 = 0;
    } else {
      e1 = A[i] / B[i];
    }
    double e2 = (S - A[i]) / (nrow - B[i]);
    
    // Calculate the variance of observations that have value smaller than x_i
    double variance_y_smaller_than_x = C[i] - 2 * A[i] * e1 + B[i] * e1 * e1;
    double variance_y_bigger_than_x = (W - C[i]) - 2 * (S - A[i]) * e2 + (nrow - B[i]) * e2 * e2;

    // The objective function that we want to minimize is the sum of those two in-grou
    // variance. Therefore we compare the current sum of variance with the minimum
    // variance that we have seen so far and update it if we see a new minimum.
    double sum_of_variance = variance_y_smaller_than_x + variance_y_bigger_than_x;
    if (sum_of_variance < min_loss) {
      min_loss = sum_of_variance;
      best_pivot = x[i];
    }
  }
  
  // The output is a R vector with two fields, best_pivot which contains the optimal
  // split point, and min_loss which corresponds to the minimal sum of variance for
  // that split point.
  return List::create(Named("splitValue") = best_pivot,
                      Named("splitError") = min_loss);
}
