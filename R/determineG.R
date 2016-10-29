#' @title Determine Groups
#' @param n number of observations in dataset
#' @param m number of successes in dataset

#' @description \code{determineG} Determines the appropriate number of groups when performing the Hosmer-Lemeshow test.
#' 
#' @return returns appropriate group size or an error upon failure 
#' 
#' @examples 
#' determineG(n=10000,m=500)
#' 
#' 
#' @details This function takes a sample size (n) and number of success (m) to determine the appropriat number of groups to use with the Hosmer-Lemeshow test.
#' 
#' @export

determineG <- function(n,m){
  if(is.numeric(n)!=TRUE) stop("n must be numeric")
  if(is.numeric(m)!=TRUE) stop("m must be numeric")
  
  other <- min(m/2, (n-m)/2, 2 + 8*(n/1000)^2)
  g <- max(10,other)
  return(g)
}

