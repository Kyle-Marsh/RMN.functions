#' @title Creates Species dataframe
#'
#' @description Takes bird data that has been prepared by bird_prepare function and creates a dataframe summerising all observations of species by year.
#'
#' @param df A data frame object
#'
#' @return A data frame that should be saved as species
#'
#' @examples species = bird_species.all(df)
#'
#' @export bird_species.all
#'
#'
bird_species.all<-function(df){


  species<-aggregate(df$Count,list(df$Spp, df$Transect, df$YEAR, df$Point),sum)
  names(species)<-c("Spp","Transect", "YEAR", "POINT","COUNT")
  species$PointYear<-as.factor(paste(species$POINT,species$YEAR, sep=""))

  return(species)

}
