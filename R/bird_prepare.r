#' @title Prepares bird data for analysis
#'
#' @description First step in the data cleaning process. Consists of 6 functions that clean and subset the data.
#'
#' @param df is a dataframe. Will take raw Point Count CSV files from Point Blue's CADC website
#'@param x is the distance you want to subset the distance bin. Same as distance in bird_subset.distance(df, distance).
#'
#' @return Cleaned and subsetted dataframe. For consistancy save as df2
#'
#' @examples df2<-bird_prepare(df)
#'
#' @export bird_prepare


bird_prepare=function(df,x){
  bird<-all_remove.first(df)
  bird<-all_Tidynames(bird)
  bird<-bird_MAPdistance.bin(bird)

  bird<-bird_prep.columns(bird)
  bird<-bird_subset.cue(bird)
  bird<-bird_subset.distance(bird, distance=x)

  return(bird)
}
