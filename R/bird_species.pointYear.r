#' @title Creates Species dataframe summarized by point and year.
#'
#' @description Takes bird data that has been prepared by bird_prepare function and creates a dataframe summerising all observations of species by year.
#'
#' @param df A data frame object
#'
#' @return A data frame
#'
#' @examples data = bird_species.pointYear(df)
#'
#' @export bird_species.pointYear
#'




bird_species.pointYear<-function(df, distance=300,transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){
  visits<-bird_visits(df)

  data<-subset(df, subset = df$Distance.Bin < distance)
  data$Distance.Bin.ID<-as.factor(data$Distance.Bin.ID)
  data = subset(data, Transect %in% transect)
  data = subset(data, YEAR %in% surveyyear)
  df<-data


  species2<-aggregate(df$Count,list(df$Spp,df$Common.Name ,df$Scientific.Name  , df$Transect, df$YEAR, df$Point),sum)
  names(species2)<-c("Spp","Common.Name", "Scientific.Name", "Transect", "YEAR", "POINT","COUNT")
  species2$PointYear<-as.factor(paste(species2$POINT,species2$YEAR, sep=""))

   species<-left_join(species2, visits, by="PointYear")
  return(species)

}