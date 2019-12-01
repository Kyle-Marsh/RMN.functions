#' @title Tidy column names in any data frame.
#'
#' @description Column names are extremely important for programs to work smoothly. This function can be used during the first stage to standardize column names. This uses the make.names function and returns the new data frame with tidy names.
#'
#' @param df A dataframe object
#'
#' @return A data frame with tidy column names
#'
#' @examples data = all_Tidynames(data)
#'
#' @export all_Tidynames
#'
#'
#'
#'
#'


all_Tidynames<-function(df){
  tidy.name.vector <- make.names(names(df), unique=TRUE)
  names(df)<-tidy.name.vector
  return(df)
}
