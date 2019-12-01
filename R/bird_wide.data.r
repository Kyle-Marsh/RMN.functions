#' @title Creates wide data frame for Point Count data
#'
#' @description For the output of add.zeros(). This function turns the dataframe from a long formate to wide formate and adds a richness column.
#'
#' @param df is a dataframe. Only works with newpc2 created by add.zeros() and it's previous steps.
#'
#' @return Data frame with the is the wide version of newpc2 with added column of Richness
#'
#' @examples bird_wide.data(newpc2)
#'
#' @export bird_wide.data
#'

bird_wide.data<-function(df){
  df2<-reshape(df, v.names="ABUNDANCE", idvar="PointYear",timevar="Spp", direction="wide")

  JustSpp<-substr(names(df2[,6:ncol(df2)]),11,14)
  colnames(df2)[6:ncol(df2)] <- JustSpp

  first<-df2[,1:5]
  second<-df2[,6:length(df2[1,])]
  second<-second[,order(colnames(second))]
  df2<-as.data.frame(cbind(first,second))


  df2$Richness<-rowSums(df2[,5:ncol(df2)] != 0)
  return(df2)
}
