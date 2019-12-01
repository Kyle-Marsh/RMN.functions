#' @title Make a percent change boxplot of soil carbon and bulk Density
#'
#' @description Creates a plot that shows average change in SOC at 2 depths and BD compared to regional average
#'
#' @param data from output of soil.final.cleanup()
#' @param transect Character string of selected ranch code
#' @param surveyyear Years for which to make summary
#' @param background = TRUE whether to display "background" data on plot not from selected ranch
#' @param legendnames specifies how points are named on the legend
#' @param legendtitle Character string of legend title
#' @param boxcolors vector of colors to display data on the boxplot
#' @param xlabels vector of strings to label boxplots
#' @param ylab character string for y axis label
#' @param choosevariables a character vector that specifies which columns in the data to plot
#'
#' @return Shows soil compaction plot
#'
#' @examples compaction.plot(soil, "ranchname")
#'
#'
#' @export percent.change.plot
#'
#'
#'



percent.change.plot = function(data, transect,
                               surveyyear = levels(as.factor(data$YEAR)),
                               background = TRUE,
                               legendnames = c(paste(transect, collapse = " "), "Others"),
                               legendtitle = "Ranch",
                               legend = TRUE,
                               boxcolors = c("black", "gray"),
                               xlabels = c("Carbon 0-10 cm", "Carbon 10-40 cm", "Bulk Density"),
                               ylab = "% Change",
                               choosevariables = c("Carbon010change", "Carbon1040change","Bulk.density.change"))
{

  library(reshape2)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  pc = function(x){(diff(x)/x[1]) * 100}




  data$YEAR = as.factor(data$YEAR)
  data = subset(data, YEAR %in% surveyyear)
  if(!background){data = subset(data, subset = Transect %in% transect)}
  duped = data$Point[duplicated(data$Point)]
  data = data[data$Point %in% duped,]
  if(!(transect %in% data$Transect)){stop("Transect does not contain multiple surveys")}
  data$YEAR = as.factor(data$YEAR)


  data2 = subset(data,select=c("Point", "Transect", "YEAR", "Carbon.0.10.cm", "Carbon.10.40.cm",
                               "Bulk.Density"))


  aggs = aggregate(data2[4:6], by = list(data2$Point), pc)
  colnames(aggs) = c("Point", "Change.Carbon0-10", "Change.Carbon10-40", "BD")
  data2 = subset(data2, data2$YEAR == max(levels(droplevels(data2$YEAR))))
  data2 = merge(data2, aggs, by= "Point", all.x = T)





  masked = data2
  masked$Transect = as.character(replace(as.character(masked$Transect),
                                         masked$Transect != transect, values = "zzzz"))
  masked$Transect = as.character(replace(as.character(masked$Transect),
                                         masked$Transect == transect, values = "aaaa"))

  masked = masked %>% arrange(desc(Transect))

  colnames(masked)[7:9] = c("Carbon010change", "Carbon1040change","Bulk.density.change")

  masked = subset(masked, select = c("Point", "Transect" , choosevariables))

  masked = melt(masked, id.vars = c("Point", "Transect"))
  masked = masked[!is.na(masked$variable),]
  masked = masked[!is.nan(masked$variable),]


  p = ggplot(masked, aes(x = variable, y = value, color = Transect)) +
    geom_boxplot() +
    scale_color_manual(values = boxcolors, labels = legendnames) +
    guides(color = ifelse(legend, guide_legend(title = legendtitle), FALSE), label = FALSE) +
    theme_bw() +
    scale_x_discrete(labels = xlabels) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    ylab(ylab) +
    xlab(NULL)

  return(p)


}




