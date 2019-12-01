#' @title Adds columns to bird dataframe
#'
#' @description Adds columns YEAR, SURVEY, Pointyear,SAMPLE and Tally
#'
#' @param df A dataframe object
#'
#' @return A dataframe prepared for data analysis with added columns.
#'
#' @examples data = bird_prep.columns(df)
#'
#' @export bird_prep.columns
#'
#'
#'
#'
bird_prep.columns<-function(df){
  df$Distance.Bin<-as.numeric(df$Distance.Bin)
  df$DATE <- mdy(df$Date)

  df$YEAR<-year(df$DATE)
  df$SURVEY<-as.factor(paste(df$Point,df$YEAR,"V",df$Visit,sep=""))
  df$PointYear<-as.factor(paste(df$Point,df$YEAR, sep=""))
  df$SAMPLE<-as.factor(paste(df$Point,df$YEAR, sep=""))
  df$Tally<-1

  return(df)
}
