#' @title Subsets point count distance to specified value
#'
#' @description Takes bird data that has been prepared and subsets the dataframe based on distance.
#'
#' @param df A data frame object
#' @param distance The distance that limit your data. 300 is max 1 is minimum.
#'
#' @return A data frame subsetted by x distance
#'
#' @examples data = bird_subset.distance(df)
#'
#' @export bird_subset.distance
#'
#'
bird_subset.distance<-function(df, distance){
  data<-subset(df, subset = df$Distance.Bin < distance)
  df$Distance.Bin.ID<-as.factor(df$Distance.Bin.ID)
  df<-data
  return(df)
}
