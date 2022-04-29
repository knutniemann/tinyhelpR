#' drop rows with diagnostic message
#'
#' @param df data.frame
#' @param idx logical, TRUE for rows to drop FALSe else
#'
#' @return df after removing rows where idx is TRUE
#' @export
#'
#' @examples
#' drop_rows(df = mtcars, idx = mtcars$cyl==4)
drop_rows<-function(df, idx){
  # stata-like drop

  # make sure idx has no missing values
  if (sum(is.na(idx))){
    stop("idx contains missing values")
  }

  # make sure idx has only logical values
  if (!is.logical(idx)){
    stop("idx is not logical")
  }

  old_rows<-nrow(df)
  drop_rows<-sum(idx)
  left_rows<-sum(!idx)

  message(paste0("dropping ", drop_rows, " rows, ", left_rows, " rows remaining"))
  df<-df[!idx,]

  return(df)
}
