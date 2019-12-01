#' @title Add date, year, and survey ID columns
#'
#' @description For soil data, adds columns with date formatted as a date object in R, year as a numeric object, and Survey as a character object that inludes the point name and the survey year
#' @description Also adds columns that display BD and Infiltration diameter, height, and volume, and one that converts infiltration times to minutes shown as a base-10 decimal numeric
#' @description Interprets date formate automatically with read.date()
#'
#' @param data
#' @param inf_diameter
#' @param bd_diameter
#' @param volume
#' @param bd_height
#'
#'
#' @return data frame with added columns
#'
#' @examples data = add_soilcolumns(data)
#' @examples x = add_soilcolumns(soildata)
#'
#' @export add.soilcolumns
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'





add.soilcolumns = function(data, inf_diameter = 15.2, bd_diameter = 5.2, volume = 450, bd_height = 7.5){



  #interpret date, isolate year as column
  data$DATE = read.date(data$Date)
  data$YEAR = year(data$DATE)
  data$SURVEY = as.factor(paste(data$Point.Name,"-",data$YEAR,sep=""))

  #
  ## add some simple data about our measurements
  data$Ring.Infiltrometer.Diameter<-inf_diameter
  data$Bulk.Density.Diameter<-bd_diameter
  data$Water.Volume<-volume
  data$Bulk.Density.Height<-bd_height
  data$Total.Volume<-pi*((data$Bulk.Density.Diameter/2)^2)*data$Bulk.Density.Height
  data$Bulk.Density.Rock.Vol[is.na(data$Bulk.Density.Rock.Vol)] = 0
  data$Bulk.Density<-(data$Bulk.Density.Dry.Wt/(data$Total.Volume-data$Bulk.Density.Rock.Vol))
  data[,"Bulk.Density"][data[,"Bulk.Density"] <= 0] <- NA

  #convert water infiltration time to minutes in base-ten decimal
  data[,"Water.Infiltration.Time.1"][data[,as.character("Water.Infiltration.Time.1")] == ""] <- "00:00:00"
  data$ISec1<-NULL
  data$ISec1<-toSeconds(as.character(data$Water.Infiltration.Time.1))
  data$ISec1<-data$ISec1/60

  return(data)


}
