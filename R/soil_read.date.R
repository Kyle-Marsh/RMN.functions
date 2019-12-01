#' @title Interpret data
#'
#' @description Interprets correctly formatted string as a date
#'
#' @param date_string
#'
#'
#' @return date object
#'
#' @examples data = read.date("data"2015-04-05")
#'
#' @export read.date
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


read.date = function(date_string){
  library(lubridate)

  if(grepl("/", date_string)){
    splitter = "/"
  } else if(grepl( "-", date_string)){
    splitter = "-"
  } else {stop("Date format not recognized")}



  date = as.character(date_string)
  x = strsplit(date, splitter)
  x = unlist(x)


  if(nchar(x[1]) <= 2){
    date_format = paste("%m", "%d", "%Y", sep = splitter)
  } else if(nchar(x[1]) == 4){
    date_format = paste("%Y", "%m", "%d", sep = splitter)
  } else {stop("Date format not recognized")}

  the_date = as.Date(date_string, format = date_format)

  return(the_date)

}
