#' @title Functional cover change summary table
#'
#' @description For veg data, summarizes change in mean cover of functional groups for each point on a property
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param transect Ranch for which to make plot
#' @param surveyyear The years for which to make the plot
#' @param invasives Boolean whether to include invasives as a functional group
#' @param type "absolute" or "relative" cover
#' @param xlab,ylab Axis Labels
#' @param casted Boolean whether to return a cast dataframe. FALSE returns a molten data frame
#'
#'
#' @return A ggplot of functional cover change with error bars
#'
#' @examples data = functional.cover.plot(lpi, type = "absolute")
#'
#' @export functional.cover.change.table
#'
#'
#'
#'



functional.cover.change.table = function(lpi,
                transect,
                invasives = FALSE,
                surveyyear = levels(as.factor(lpi$year)),
                type = "absolute",
                casted = TRUE)

{

  library(ggplot2)
  library(reshape2)

  cov = functional.cover.table(lpi, type = type, transect = transect,
                               surveyyear = surveyyear, invasives = invasives, includemeta = TRUE)
  cov$NumIndices = NULL
  cov = melt(cov, id = c("Transect", "pointyear", "Point.Id", "year"))
  names(cov)<-c("Transect", "pointyear", "Point.Id", "year", "Type", "Cover")

  cov$year = droplevels(as.factor(cov$year))
  cov$Type = as.factor(cov$Type)


  d2 = subset(cov, subset = year == max(levels(year)))
  d1 = subset(cov, subset = year == min(levels(year)))

  coveryear = merge(d1, d2, by = c("Point.Id", "Type"))
  coveryear$change = coveryear$Cover.y - coveryear$Cover.x

  if(casted){coveryear = dcast(coveryear, Point.Id ~ Type, value.var = "change")}

  return(coveryear)

}




