#' @title Plot species richness and cover
#'
#' @description For veg data, plots cover of shrubs, trees, litter, thatch, bare ground for each year in which data were collected.
#' @description  Tree and shrub covers come from releve estimates.
#' @description NOTE: Thatch was only measured starting in 2017.
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param releve A dataframe object of releve data from a veg survey
#' @param transect Ranch for which to make the plot
#' @param surveyyear The years for which to make the table
#' @param choose.variable Character vector that identifies which variables to summarize. See cover.summary()
#' @param plot.name Character vector of names of plots. If choose.variable is manually changed, then plot.names also needs to be changed.
#' @param multiple_years Boolean that determines whether multiple years of data are plotted. if FALSE, only most recent year is plotted
#' @param barcolors Character vector of bar fill colors for each year of data
#' @param legend Boolean that specifies whether to display a legend
#' @param legendname A title for your legend
#' @param xlab,ylab Labels for axes
#' @param xangle Angle to display point names on x axis
#'
#' @return ggplot object
#'
#' @examples data = cover.sum.plot(lpi, checklist)
#'
#' @export cover.sum.plot
#'
#'


cover.sum.plot = function(lpi, releve,
                          transect,
                          surveyyear = c(levels(as.factor(lpi$year)), levels(as.factor(checklist$year))),
                          choose.variable = c("SpeciesRichness", "Litter", "Thatch", "BareGround",
                                              "Trees", "Shrubs"),
                          plot.names = c("Species Richness", "Litter", "Thatch", "Bare Ground",
                                         "Trees", "Shrubs"),
                          multiple_years = TRUE,
                          barcolors = c("olivedrab", "steelblue3", "mediumpurple4", "orange3", "tan2"),
                          legend = TRUE,
                          legendname = "Year",
                          xlab = "Point ID",
                          ylab = "Value",
                          xangle = 45
                          )
  {
  library(ggplot2)
  library(plyr)
  library(dplyr)
  library(reshape2)
  datasum = cover.summary(lpi = lpi, releve = releve, transect = transect,
                          choose.variable = choose.variable, surveyyear = surveyyear, rounded = FALSE)
  datasum$NumIndices = NULL

  if(isFALSE(multiple_years)){datasum = subset(datasum, subset = datasum$year == max(datasum$year))}

  datasum_melt = melt(datasum, id = c("pointyear", "PointId", "year"))
  levels(datasum_melt$variable) = plot.names

  p = ggplot(datasum_melt, aes(x = PointId, y = value, fill = year)) +
    geom_bar(stat = "identity",position = position_dodge(), color = "black") +
    scale_fill_manual(values= barcolors, name = legendname) +
    facet_wrap(~variable) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = xangle, hjust = 1), legend.position = ifelse(legend, "right", "none")) +
    xlab(xlab) +
    ylab(ylab)


  return(p)

}
