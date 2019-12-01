#' @title Create a table of cover for each species at each point
#'
#' @description For veg data, summarizes cover of each species at each point.
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param type Denotes type of cover to calculate"absolute" or "relative"
#' @param transect Ranch for which to summarize cover
#' @param surveyyear The year for which to generate the summary
#'
#' @return A dataframe summary of cover of species
#'
#' @examples data = species.cover.table(lpi, type = "relative")
#'
#' @export species.cover.table
#'
#'

species.cover.table = function(lpi,
                               transect,
                               type = "absolute",
                               surveyyear = max(levels(as.factor(lpi$year))))
  {
  library(reshape2)
  library(dplyr)
  lpi$Tally = 1
  lpi$year = as.factor(lpi$year)
  lpi = subset(lpi, subset = lpi$year %in% surveyyear)
  lpi = subset(lpi, subset = lpi$Transect.Name %in% transect)

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


  newlpi = dcast(longlpi, pointyear~Spp,value.var=c("Tally"), sum)
  newlpi = merge(newlpi, a, by="pointyear")
  pointyear = newlpi$pointyear
  firstdrops = c("Var.2", "NA", "NOPLANT", "M", "L", "EM", "AM", "R", "WL", "S")
  newlpi = newlpi[,!(names(newlpi) %in% firstdrops)]

  if(type == "absolute"){
    newlpi.relative = (newlpi[,3:ncol(newlpi)-1]/newlpi$NumIndices) *100
  } else if(type == "relative"){
    newlpi.relative = newlpi[,3:ncol(newlpi)-1]/rowSums(newlpi[,3:ncol(newlpi)-1], na.rm=TRUE) * 100
  } else{ stop("Argument for type not recognized") }

  newlpi.relative = cbind(pointyear, newlpi.relative)

  ## Almost there, just some tidying

  drops = c("Var.2", "NA", "NumIndices", "NOPLANT", "UNKNWN", "M", "L", "EM", "AM", "R", "WL", "S")
  newlpi.relative = newlpi.relative[,!(names(newlpi.relative) %in% drops)]
  newlpi = newlpi[,!(names(newlpi) %in% drops)]

  t.lpirelative = as.data.frame(t(newlpi.relative[,-1]))
  colnames(t.lpirelative) = newlpi.relative$pointyear


  t.lpirelative$mean_cover = rowMeans(t.lpirelative, na.rm = TRUE)
  rownames(t.lpirelative) = colnames(newlpi.relative)[-1]
  t.lpirelative$species = rownames(t.lpirelative)
  t.lpirelative = arrange(t.lpirelative, desc(mean_cover))

  t.lpirelative2 = t.lpirelative[,c(ncol(t.lpirelative), 1:(ncol(t.lpirelative) - 1))]
  t.lpirelative2[,2:ncol(t.lpirelative2)] = round(t.lpirelative2[,5:ncol(t.lpirelative2)], 2)
  return(t.lpirelative2)



}
