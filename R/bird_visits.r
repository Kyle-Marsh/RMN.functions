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


bird_visits<-function(df){
  a<-subset(df,duplicated(df$SURVEY)==FALSE)
  visits<-aggregate(a$Tally, list(a$PointYear), sum)
  names(visits)<-c("PointYear", "Visits")
  return(visits)
}
