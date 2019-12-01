#' @title Adds year and pointyear columns to data
#'
#' @description For veg data, formats column with date as a date object in R, adds a column for year as a numeric object, and pointyear as a character object that inludes the point name and the survey year
#' @description Data supplied needs to have column named "PointId" or "Point.Id" and a date column named "Date" or "Event.Date"
#' @description Makes use of package function read.date()
#' @description Removes empty rows in LPI
#'
#' @param data A dataframe object
#'
#'
#' @return Dataframe with added columns
#'
#' @examples data = add.pointyear(data)
#'
#' @export add.pointyear
#'
#'

add.pointyear = function(data){

  if("Soil.Surface" %in% colnames(data)){
    data = subset(data, subset = data$Soil.Surface != "")
  }

  if("Date" %in% colnames(data)){
    data$Date = read.date(data$Date)
    data$year = format(as.Date(data$Date), "%Y")
  } else if("Event.Date" %in% colnames(data)){
    data$Event.Date = read.date(data$Event.Date)
    data$year = format(as.Date(data$Event.Date), "%Y")
  }

  if("PointId" %in% colnames(data)){
    data$pointyear = paste(data$PointId, "-", data$year)
  } else if("Point.Id" %in% colnames(data)){
    data$pointyear = paste(data$Point.Id, "-", data$year)
  } else {stop("No Point.Id column identified")}

  return(data)
}
