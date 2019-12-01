#' @title Functional cover summary plot
#'
#' @description For veg data, summarizes and plots mean cover of functional groups.
#' @description Data are summarized for all years specified.
#' @description Background data are supplied from the lpi data select. Data for a selected ranch are not used in the calculation of regional averages.
#'
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param transect Ranch for which to make plot
#' @param type "absolute" or "relative" cover
#' @param surveyyear The year for which to calculate cover
#' @param background = TRUE whether to display "background" data on plot not from selected ranch
#' @param xlab,ylab Axis labels
#' @param legendnames specifies how points are named on the legend
#' @param legendtitle Character string of legend title
#' @param boxcolors vector of colors to display data on the boxplot
#'
#' @return A ggplot of functional cover with error bars
#'
#' @examples data = functional.cover.plot(lpi, type = "absolute")
#'
#' @export functional.cover.plot
#'
#'
#'
#'


functional.cover.plot = function(lpi,
               type = "absolute",
               transect,
               background = TRUE,
               invasives = FALSE,
               surveyyear = max(levels(as.factor(lpi$year))),
               xlab = "Functional Group",
               ylab = "Percent Cover",
               legendtitle = "Ranch",
               legendnames = c(paste(transect, collapse = " "), "Others"),
               boxcolors = c("black","gray")){
  library(ggplot2)

  cov = functional.cover.table(lpi, type = type, transect = levels(lpi$Transect.Name),
                               surveyyear = surveyyear, invasives = invasives,
                               includemeta = TRUE)
  if(!background){cov = subset(cov, subset = Transect %in% transect)}
  cov$NumIndices = NULL


  cov = melt(cov, id = c("Transect" ,"pointyear", "Point.Id", "year"))

  names(cov)<-c("Transect", "pointyear", "Point.Id", "year", "Type", "Cover")
  masked = cov
  masked$Transect = as.character(replace(as.character(masked$Transect),
                                         masked$Transect != transect, values = "zzzz"))
  masked$Transect = as.character(replace(as.character(masked$Transect),
                                         masked$Transect == transect, values = "aaaa"))

  masked$year = as.factor(masked$year)
  masked$Type = as.factor(masked$Type)


  cover_plot = ggplot(masked, aes(x = Type, y = Cover, color = Transect)) +
    geom_boxplot() +
    scale_color_manual(name = legendtitle, values = boxcolors, labels = legendnames) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(xlab) +
    ylab(ylab)

  return(cover_plot)

}
