#' @title Carbon Plot
#'
#' @description Plots soil carbon as scatterplot. Points from ranch identified are black, and all others are grayed out. Points are labeled automatically unless specified with labels = FALSE
#'
#' @param data from output of soil.final.cleanup()
#' @param transect Character string of selected ranch code
#' @param year The year or years for which to make the plot
#' @param labels = TRUE determines whether points from the chosen ranch are labeled on the plot
#' @param background = TRUE whether to display "background" data on plot not from selected ranch
#' @param pointcolors = c("black", "gray") a vector that specifies colors of points on the plot
#' @param legend specifies whether to display a legend
#' @param legendnames specifies how points are named on the legend
#' @param legendtitle title for the legend
#' @param box.padding numeric adjusts spacing of labels on the plot
#' @param xlab,ylab character strings for axis lables
#' @param pointsize specifies size of a point
#'
#'
#' @return plot of soil carbon data
#'
#' @examples carbon.plot(soil, "ranchname")
#'
#' @export carbon.plot
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


carbon.plot<-function(data, transect, year,
                      labels = TRUE,
                      background = TRUE,
                      pointcolors = c(rep("black", length(transect)),"gray"),
                      legend = TRUE,
                      legendnames = c(transect, "Others"),
                      legendtitle = "Ranch",
                      box.padding = 0.5,
                      xlab = "% Carbon 0-10 cm",
                      ylab = "% Carbon 10-40 cm",
                      pointsize = 2){

  library(ggplot2)
  library(ggrepel)
  library(dplyr)

  data = subset(data, data$YEAR %in% year)
  transect = transect[transect %in% data$Transect]
  if(!background){data = subset(data, subset = Transect %in% transect)}
  masked = data
  masked$Transect = as.character(masked$Transect)
  masked$Transect[!(masked$Transect %in% transect)] = "zzzz"


  masked2 = masked[!is.na(masked$Carbon.0.10.cm) & !is.na(masked$Carbon.10.40.cm),]


  masked_soil = arrange(masked2, desc(Transect))


  if(all(masked_soil$Transect == "zzzz")){
    pointcolors = pointcolors[length(pointcolors)]
    legendnames = legendnames[length(legendnames)]
  }

  if(isTRUE(labels)){
    labs = as.character(masked_soil$Point[masked_soil$Transect %in% transect])
  } else {
    labs = NA
  }


  p = ggplot(masked_soil, aes(x = Carbon.10.40.cm, y = Carbon.0.10.cm, color = Transect)) +
    geom_point(size = pointsize) +
    scale_color_manual(values = pointcolors, labels = legendnames) +
    theme_bw() +
    xlab(xlab) +
    ylab(ylab) +
    guides(color = ifelse(legend, guide_legend(title = legendtitle), FALSE), label = FALSE) +
    geom_label_repel(data = masked_soil[masked_soil$Transect %in% transect,], aes(label = labs), box.padding = box.padding, show.legend = FALSE)

  return(p)

}
