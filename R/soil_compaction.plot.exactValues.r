#' @title Make a soil compaction plot with exact values
#'
#' @description From output of  creates a soil compaction plot displaying BD and Infiltration data, and compares to other ranches in the region
#'
#' @param data dataframe from output of soil.final.cleanup()
#' @param transect Character string of selected ranch code
#' @param background = TRUE whether to display "background" data on plot not from selected ranch
#' @param labels = TRUE determines whether points from the chosen ranch are labeled on the plot
#' @param pointcolors = c("black", "gray") a vector that specifies colors of points on the plot
#' @param legend specifies whether to display a legend
#' @param legendnames specifies how points are named on the legend
#' @param legendtitle title for the legend
#' @param box.padding numeric adjusts spacing of labels on the plot
#' @param xlab,ylab character strings for axis lables
#' @param linetype specifies type of line shown for BD and Infiltration targets
#' @param pointsize specifies size of a point
#' @param xlims,ylims numeric vectors of length 2 that specify the respective lower and upper limits of the axes. NA values cause default axis limits
#'
#' @return Shows soil compaction plot
#'
#' @examples compaction.plot.exactValue(soil, "ranchname")
#'
#'
<<<<<<< HEAD
#' @export compaction.plot.exactValues
=======
#' @export compaction.plot.exactValue
>>>>>>> 155b2dc75d607d67178530fdeea5105a340ebb0f
#'
#'


compaction.plot.exactValues<-function(data,
                     transect,
                     year,
                     background = TRUE,
                     labels = TRUE,
                     pointcolors = c(rep("black", length(transect)),"gray"),
                     legend = TRUE,
                     legendnames = c(paste(c(transect)), "Others"),
                     legendtitle = "Ranch",
                     xlab = "Bulk density (g/cm3)",
                     ylab = "Water Infiltration Rate (minutes)",
                     box.padding = 0.5,
                     pointsize = 2,
                     linetype = "dashed",
                     xlims = c(NA, NA),
                     ylims = c(NA, NA)){
  library(reshape2)
  library(dplyr)
  library(ggplot2)
  library(soiltexture)
  library(ggrepel)
  year2<- year
  data = subset(data, YEAR %in% year)
  if(any(is.na(data$BD_dist))){
    removed = nrow(data[is.na(data$BD_dist) | is.na(data$Infilt_dist),])
    warning(paste(removed, "Observations missing bulk density data have been removed"))
    data1 = data[!is.na(data$BD_dist),]
    data1 = data[!is.na(data$Infilt_dist),]
  } else {
    data1 = data
  }
  transect = transect[transect %in% data$Transect]
  if(!background){data = subset(data, subset = Transect %in% transect)}

  if(isTRUE(labels)){
    labs = as.character(data$Point[data$Transect %in% transect])
  } else {
    labs = NA
  }
  p = ggplot(data, aes(x=Bulk.Density, y=Infilt1))+
    geom_point()+
    ylab("Water Infiltration (minutes)")+
    xlab("Bulk Density (g/cm3)")+
    geom_hline(yintercept = 15)+
    geom_vline(xintercept = 1.4)+
    ylim(0,max(data$Infilt1)+5)+
    xlim(min(data$Bulk.Density)-.25, max(data$Bulk.Density)+.25)+
    geom_text(aes(label=as.character(data$Point)),position=position_jitter(height = 1.5))+
    ggtitle(paste("Soil Compaction Plot", as.character(year)))
  return(p)
}


