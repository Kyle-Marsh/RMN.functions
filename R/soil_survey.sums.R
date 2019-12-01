#' @title Summarize data for soil survey visits
#'
#' @description For a dataframe output from function add_soilcolumns(), produces a table that shows mean BD and Infiltration measurements
#'
#' @param data
#'
#' @return Data frame with One row per survey event, includes survey point names, date info, soil texture data, carbon % data, mean BD, and mean Infiltration time
#'
#' @examples survey_sums(soildata)
#'
#' @export survey.sums
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



survey.sums = function(data){

  data1 = data

  data1[data1 == 0]<- NA
  filtmean<-aggregate(data1$ISec1, by=list(data1$SURVEY), mean, na.rm=TRUE)
  bulkmean<-aggregate(data1$Bulk.Density, by=list(data1$SURVEY), mean, na.rm=TRUE)
  names(bulkmean)<-c("SURVEY", "Bulk.Density")
  names(filtmean)<-c("SURVEY","Infilt1")

  carbon<-subset(data1, select=c("Transect.Name","Point.Name","SURVEY","YEAR","Carbon.0.10.cm", "Carbon.10.40.cm","Clay.10.40.cm","Sand.10.40.cm","Silt.10.40.cm"))
  names(carbon)<-c("Transect","Point","SURVEY","YEAR","Carbon.0.10.cm", "Carbon.10.40.cm","Clay.10.40.cm","Sand.10.40.cm","Silt.10.40.cm")
  carbon<-unique(carbon)

  ##Merge water infiltration, bulkdensity, and carbon together##
  newdata<-merge(filtmean, bulkmean, by="SURVEY")
  newdata<-merge(newdata, carbon, by="SURVEY")






  return(newdata)


}
