#' @title Creates Species dataframe with Spp codes
#'
#' @description Takes bird data that has been prepared by bird_prepare function and creates a dataframe summerising all observations of species by year.
#'
#' @param df A data frame object
#'
#' @return A data frame that should be saved as species2
#'
#' @examples species2 = bird_species.narrow(df)
#'
#' @export bird_species.narrow
#'






bird_species.narrow<-function(df,distance=300,transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){

  visits<-bird_visits(df)

  data<-subset(df, subset = df$Distance.Bin < distance)
  data$Distance.Bin.ID<-as.factor(data$Distance.Bin.ID)
  data = subset(data, Transect %in% transect)
  data = subset(data, YEAR %in% surveyyear)
  df<-data

  species<-aggregate(df$Count,list(df$Spp, df$Transect, df$YEAR, df$Point),sum)
  names(species)<-c("Spp","Transect", "YEAR", "POINT","COUNT")
  species$PointYear<-as.factor(paste(species$POINT,species$YEAR, sep=""))

  species<-left_join(species, visits, by="PointYear")


  return(species)

}