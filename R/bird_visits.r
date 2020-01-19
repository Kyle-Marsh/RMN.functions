#' @title Create Visit Table
#'
#' @description Takes bird data that has been prepared by bird_prepare function and creates a dataframe of visits per point.
#'
#' @param df A data frame object
#'
#' @return A data frame with visits
#'
#' @examples data = bird_visits(df)
#'
#' @export bird_visits
#'
#'


bird_visits<-function(df, distance =300,transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){
  data<-subset(df, subset = df$Distance.Bin <= distance)
  data$Distance.Bin.ID<-as.factor(data$Distance.Bin.ID)
  data = subset(data, Transect %in% transect)
  data = subset(data, YEAR %in% surveyyear)
  df2<-data
  a<-subset(df2,duplicated(df2$SURVEY)==FALSE)
  visits<-aggregate(a$Tally, list(a$PointYear), sum)
  names(visits)<-c("PointYear", "Visits")
  return(visits)
}

