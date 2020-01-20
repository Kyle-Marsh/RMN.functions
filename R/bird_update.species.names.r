#' @title Update species names.
#'
#' @description changes the species list names to change WESJ to CASJ.
#'
#' @param df A data frame object
#'
#' @return A data frame that is subsetted based on detection
#'
#' @examples data = bird_updateSpeciesNames(df)
#'
#' @export bird_updateSpeciesNames
#'
#'

bird_updateSpeciesNames<-function(df){


  df$Spp[df$YEAR < 2016 & df$Spp=="WESJ"]<-"CASJ"
  df$Common.Name[df$YEAR < 2016 & df$Common.Name=="Western Scrub-Jay"]<-"California Scrub-Jay"

  return(df)
}
