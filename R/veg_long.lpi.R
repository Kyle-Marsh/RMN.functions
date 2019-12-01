#' @title Summarize number of lpi hits by species
#'
#' @description For veg data, summarizes number of hits of each species at each point. Intended as intermediate data prep step
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#'
#' @return A dataframe summary of species hits at each survey
#'
#' @examples summart = lpi.summary(lpi)
#'
#'
#' @export long.lpi
#'
#'
#'

long.lpi = function(lpi){
  library(reshape2)
  lpi$Tally = 1

  a = aggregate(lpi$Tally, list(lpi$pointyear), sum)
  names(a) = c("pointyear", "NumIndices")

  lpi.trim = subset(lpi, select=c("pointyear", "year", "Point.Id", "Canopy1", "Canopy2", "Canopy3",
                                  "Top.Layer","Lower1","Lower2", "Lower3", "Lower4","Lower5","Lower6","Lower7",
                                  "Lower8","Lower9","Lower10","Soil.Surface"))

  longlpi = melt(lpi.trim, id=c("pointyear", "Point.Id", "year"))
  # Note, this step may give a warning, but it's
  names(longlpi) = c("pointyear", "Point.Id", "year", "Layer", "Spp")

  longlpi = subset(longlpi, select=c("pointyear", "Point.Id", "year", "Spp"))
  longlpi$Tally = 1


  return(longlpi)

}
