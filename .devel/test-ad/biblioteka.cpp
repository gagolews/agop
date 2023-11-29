#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
double exp_test(NumericVector x){
  int n = x.size();
  NumericVector z = 1-exp(-x/mean(x));
  z.sort();
  double w_pom = 0.0;
  for(int i = 0; i<n; i++){
    w_pom += (2.0*i+1)*(log(z[i])+log(1-z[n-i-1]));
  }
  return -n-w_pom/n;
}
