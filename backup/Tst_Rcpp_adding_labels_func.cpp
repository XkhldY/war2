#include <Rcpp.h>
// #include <cstdlib>
// #include <iostream>
#include <algorithm> // for std::find
#include <iterator> // for std::begin, std::end
using namespace Rcpp;
using namespace std;
// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
DataFrame timesTwo(DataFrame x) {
  IntegerVector  a=x["a"];
  
  return DataFrame::create(_["a"]= a);
}

// [[Rcpp::export]]
NumericVector tst(DataFrame x) {
  NumericVector a=x["location_id"];
  NumericVector user=x["user"];
  NumericVector label=x["label"];
  NumericVector n=a.size();
  NumericVector z(n);
  bool exists;
  for(int i=0;i<z.size();i++)
  {
    
    for(int j=i;j<z.size();j++)
    {
      
      exists = std::find(z.begin(), z.end(), a[j]) != z.end();
      
      if(exists)
      {
        label[j]=0;
      }
      z[j]=a[j];
      
      if(user[j]!=user[j+1]){
        i=j+1;
        cout<<user[j]<<"\n";
        break;
      }
    }
  }
  return (label);
}

// [[Rcpp::export]]
DataFrame tst2(DataFrame x) {
  NumericVector a=x["location_id"];
  NumericVector user=x["user"];
  NumericVector label=x["label"];
  NumericVector n=a.size();
  NumericVector z(n);
  bool exists;
  
  for(int j=0;j<z.size();j++)
  {
    
    exists = std::find(z.begin(), z.end(), a[j]) != z.end();
    
    if(exists)
    {
      label[j]=0;
    }
    z[j]=a[j];
    
    if(user[j]!=user[j+1]){
      cout<<user[j]<<"\n";
      break;
    }
  }
  x["label"]=label;
  return x;
}

// func<-function(x)
// {
//   x=data.table(x)
//   z<-x[1,]$location_id
//   print(unique(x$user))
//   for(i in 2:nrow(x))
//   {
//     if(x[i,]$location_id %in% z)
//       x[i,]$label<-0
//     else
//       z=rbind(z,x[i,]$location_id)
//   }
//   
//   
//   return (x)
// }
// [[Rcpp::export]]
RcppExport SEXP comp(SEXP x, SEXP y){
  int i,n;
  Rcpp::NumericVector vector1(x);
  Rcpp::NumericVector vector2(y);
  n=vector2.size();
  Rcpp::NumericVector product(n);
  for(i=0;i<n;i++){
    product[i]=vector1[i]*vector2[i];
  }
  return(product);
}
;

/*** R
d=data.frame(a=1:1000)
  timesTwo(d)
# data_50more_wlabels$label=1
  data$label=1
# system.time(m<-tst(data))
# system.time(x<-ddply(data_50more[data_50more$user<10,],.(user),tst))
# x=rnorm(100,0,1)
# y=rnorm(100,0,1)
#   .Call('comp',x,y)
*/
