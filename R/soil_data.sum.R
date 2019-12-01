#' @title Make a table to summarize data
#'
#' @description From output of soil.final.cleanup() creates a soil texture plot and compares a ranch to other ranches in the dataset
#'
#' @param data dataframe, output of soil.final.cleanup()
#' @param transect Character string of selected ranch code
#' @param surveyyear Years for which to use data
#'
#' @return Shows soil data in a nice-looking table
#'
#' @examples data.sum(soil, "ranchname")
#'
#'
#' @export data.sum
#'
#' @return data summary table
#'


data.sum = function(data, transect,
                    surveyyear = levels(as.factor(data$YEAR)),
                    return_df = FALSE){

  library(gridExtra)
  library(stats)
  library(dplyr)

  data1 = subset(data, data$Transect %in% transect)
  data1 = subset(data1, data1$YEAR %in% surveyyear)
  data1$YEAR = as.factor(data1$YEAR)
  if(nrow(data1) == 0){
    warning("NULL object returned - no rows to aggregate")
    return(NULL)
    } else {

  pc = function(x){(diff(x)/x[1]) * 100}

  if(nlevels(as.factor(surveyyear)) == 1){

    data2 = subset(data1,select=c("Point","Carbon.0.10.cm", "Carbon.10.40.cm",
                                  "Bulk.Density", "Infilt1"))



    data2[,2:5] = round(data2[,2:5], 2)


  } else if(nlevels(as.factor(surveyyear > 1))){
    data2 = subset(data1,select=c("Point", "YEAR", "Carbon.0.10.cm", "Carbon.10.40.cm",
                                  "Bulk.Density", "Infilt1"))

    if(nrow(data2) == 0){return(NULL)}

    aggs = aggregate(data2[3:5], by = list(data2$Point), pc)

    for(i in 2:4){

      aggs[,i] = as.numeric(aggs[,i])

    }

    colnames(aggs) = c("Point", "Change.Carbon0-10", "Change.Carbon10-40", "BD")
    data2 = subset(data2, YEAR == max(levels(data2$YEAR)))
    data2 = merge(data2, aggs, by= "Point")
    data2$YEAR = NULL

    data2[,2:8] = round(data2[,2:8], 2)
  }


  if(ncol(data2) == 8 & !return_df){
    colnames(data2) = c("Point","Carbon\n0-10cm", "Carbon\n10-40cm",
                        "Bulk\nDensity", "Average\nInfiltration\n(min)",
                        "Carbon\n% Change\n0-10cm","Carbon\n% Change\n10-40cm",
                        "Bulk\nDensity\n% change")
  } else if (ncol(data2) == 5 & !return_df){
    colnames(data2) = c("Point","Carbon\n0-10cm", "Carbon\n10-40cm", "Bulk\nDensity", "Average\nInfiltration\n(min)")
  }


  if(!return_df){
    grid.table(data2,theme= ttheme_default(base_size=10), rows = NULL)
  } else {return(data2)}


  }



}
