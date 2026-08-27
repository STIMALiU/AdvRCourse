binary_search_R <- function(sortedNumVector, searchTerm, lengthOfVector) {
  
  L = 1
  R = lengthOfVector
  
  while (L <= R) {
    
    m = floor((L + R)/2)
    
    if (sortedNumVector[m] < searchTerm) {
      
      L = m + 1
      
    } else if (sortedNumVector[m] > searchTerm) {
      
      R = m - 1
    
    } else if (sortedNumVector[m] == searchTerm) {
      
      return(m)
      
    }
      
  }
  
  return(-1)
  
}

library(Rcpp)
cppFunction(code = 'int binary_search_Cpp(NumericVector sortedNumVector, double searchTerm, int lengthOfVector)
{
  int L = 0;
  int R = lengthOfVector - 1;
  
  while (L <= R) {
    
    int m = floor((L + R)/2);
    
    if (sortedNumVector[m] < searchTerm) {
      
      L = m + 1;
      
    } else if (sortedNumVector[m] > searchTerm) {
      
      R = m - 1;
      
    } else if (sortedNumVector[m] == searchTerm) {
      
      return(m + 1);
      
    }
    
  }
    
  return(-1);
  
}')

x <- rnorm(1000)
sorted_x <- x[order(x)]

library(microbenchmark)
microbenchmark(times=3000, 
               binary_search_R(sorted_x, 56, 1000), 
               binary_search_Cpp(sorted_x, 56, 1000))
