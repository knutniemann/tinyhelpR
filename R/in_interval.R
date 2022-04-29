#' Tag elements inside interval
#'
#' @param vec numeric
#' @param lower numeric, lower bound
#' @param upper numeric, upper bound
#' @param lower_incl # logical, lower bound inside interval?
#' @param upper_incl # logical, upper bound inside interval?
#' @param missing_false # logical, if TRUE missing values in vec will be tagged as FALSE
#'
#' @return logical, TRUE when elements from vec are inside interval
#' @export
#'
#' @examples in_interval(vec = 1:4, lower = 2, upper= 3, lower_incl=TRUE, upper_incl=TRUE)
in_interval<-function(vec, lower, upper, lower_incl, upper_incl, missing_false=FALSE){
  # check if vec is within lower and upper

  if (lower_incl){
    lower_fun<-function(x) x>=lower
  } else {
    lower_fun<-function(x) x>lower
  }

  if (upper_incl){
    upper_fun<-function(x) x<=upper
  } else {
    upper_fun<-function(x) x<upper
  }

  out<-lower_fun(vec) & upper_fun(vec)

  if (missing_false){
    out[is.na(out)]<-FALSE
  }

  return(out)

}
