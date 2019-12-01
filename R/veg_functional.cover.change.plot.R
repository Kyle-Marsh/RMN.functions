#' @title Functional cover change summary plot
#'
#' @description For veg data, summarizes change in mean cover of functional groups for a property
#' @description This will use data from the FIRST and LAST year specified for which there is lpi data on the selected ranch
#' @description Regional comparison data will use these same years to calculate average change
#' @description EX:: lpi data covers 2015-2019. Ranch XXXX has data from 2016 and 2019. Regional averages will therefore only include other properties that were sampled in 2016 and 2019. Ranches surveyed in 2015 and 2018 are not included in the regional calculation.
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param transect Ranch for which to make plot
#' @param invasives Boolean whether to include invasives as a functional group
#' @param surveyyear The years for which to make the plot
#' @param background = TRUE whether to display "background" data on plot not from selected ranch
#' @param type "absolute" or "relative" cover
#' @param xlab,ylab Axis Labels
#' @param legendnames specifies how points are named on the legend
#' @param legendtitle Character string of legend title
#' @param boxcolors vector of colors to display data on the boxplot
#'
#'
#' @return A ggplot of functional cover change with error bars
#'
#' @examples data = functional.cover.plot(lpi, type = "absolute")
#'
#' @export functional.cover.change.plot
#'
#'
#'
#'


functional.cover.change.plot = function(lpi,
               transect,
               invasives = FALSE,
               surveyyear = levels(as.factor(lpi$year)),
               background = TRUE,
               type = "absolute",
               xlab = "Functional Group",
               ylab = paste("Cover Change",
                            " ",
                            min(levels(as.factor(surveyyear))),
                            "-",
                            max(levels(as.factor(surveyyear))),
                            sep = ""),
               legendtitle = "Ranch",
               legendnames = c(paste(transect, collapse = " "), "Others"),
               boxcolors = c("black","gray")
){

  library(ggplot2)
  library(reshape2)

  ranchlpi = subset(lpi, subset = lpi$Transect.Name %in% transect)
  ranchlpi$year = droplevels(as.factor(ranchlpi$year))
  surveyyear = surveyyear[surveyyear %in% ranchlpi$year]


  coveryear = functional.cover.change.table(lpi = lpi,
                                            transect = levels(lpi$Transect.Name),
                                            type = type,
                                            invasives = invasives,
                                            surveyyear = surveyyear,
                                            casted = FALSE
  )
  masked = coveryear
  if(!background){masked = subset(masked, subset = Transect.x %in% transect)}

  masked$Transect.x = as.character(replace(as.character(masked$Transect.x),
                                           masked$Transect.x != transect, values = "zzzz"))
  masked$Transect.x = as.character(replace(as.character(masked$Transect.x),
                                           masked$Transect.x == transect, values = "aaaa"))
  if(!("aaaa" %in% masked$Transect.x)){stop("This ranch does not contain multiple surveys in the years selected")}



  c = ggplot(masked, aes(x = Type, y = change, color = Transect.x)) +
    geom_boxplot() +
    scale_color_manual(name = legendtitle, values = boxcolors, labels = legendnames) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(label = xlab) +
    ylab(label = ylab) +
    geom_hline(yintercept = 0, linetype = 'dotted')

  return(c)


}
