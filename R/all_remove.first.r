#' @title Removes first row of df if it is missing.
#'
#' @description CADC will add an empty row when downloading data.
#'
#' @param df A dataframe object
#'
#' @return A data frame without empty first row
#'
#' @examples data = all_remove.first(data)
#'
#' @export all_remove.first
#'
#'
#'
#'
#'







all_remove.first<-function(df){
  if(df[1,1]=="")
  {df<-df[-1,]
  rownames(df)<-1:nrow(df)
  return(df)
  } else { df<-df}}
