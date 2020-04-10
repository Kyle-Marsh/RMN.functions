#' @title Remove first row of data frame if empty
#'
#' @description A reoccuring problem when importing data frames into rstudio is a blank first row that is added to the data frame. This function looks at the firsts row to see if the first cell has an empty value. If it does then it will removes the row and return the dataset.
#'
#' @param df A dataframe object
#'
#' @return A data frame without a blank first row.
#'
#' @examples data = remove_first(data)
#'
#' @export all_remove.first2
#'
#'
#'
#'



all_remove.first2<-function(df){
  if(df[1,1]=="")
  {df<-df[-1,]
  rownames(df)<-1:nrow(df)
  return(df)
  } else { df<-df}}

