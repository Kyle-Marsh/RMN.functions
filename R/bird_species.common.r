#' @title Creates Species dataframe with common names
#'
#' @description Takes bird data that has been prepared by bird_prepare function and creates a dataframe summerising all observations of species by year. Different than bird_species.all because it adds common and scientific names of birds.
#'
#' @param df A data frame object
#'
#' @return A data frame that should be saved as species2
#'
#' @examples species2 = bird_species.common(df)
#'
#' @export bird_species.common
#'

bird_species.common<-function(df){



  species2<-aggregate(df$Count,list(df$Spp,df$Common.Name ,df$Scientific.Name  , df$Transect, df$YEAR, df$Point),sum)
  names(species2)<-c("Spp","Common.Name", "Scientific.Name", "Transect", "YEAR", "POINT","COUNT")
  species2$PointYear<-as.factor(paste(species2$POINT,species2$YEAR, sep=""))

  return(species2)}
