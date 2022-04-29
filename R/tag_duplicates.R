#' Tag duplicates and give short diagnostic message
#'
#' @param df data.frame to be checked for duplicates
#' @param ... further options for base::duplicated
#'
#' @return logical, equal to 1 if row is duplicated, 0 else
#' @export
#'
#' @examples
#' tag_duplicates(c(9:20, 1:5, 3:7, 0:8))
tag_duplicates<-function(df, ...){
  # check df for duplicates, return a summary message and return idx of duplicates

  idx_dupl<-duplicated(df, ...)
  n_dupl<-sum(idx_dupl)
  varnames<-names(df)

  message("total of ", n_dupl, " duplicates")

  return(idx_dupl)
}
