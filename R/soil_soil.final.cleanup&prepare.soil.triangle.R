#' @title Prepare soil data to make figures
#'
#' @description For a dataframe output from function pair.data(), produces a table that shows 2018 data and amount of change.
#' @description This table is designed to be the source data for our functions that make figures
#' @description prepare.soil.triangle() takes the result of soil.final.cleanup and adds necessary columns to make a soil triangle
#'
#' @param paired_data from output of pair.data
#' @param first = FALSE if TRUE will use observations only from the first surveys of each point
#' @param data in prepare.soil.triangle() from output of soil.final.cleanup()
#' @param Inftarget numeric, target infiltration time
#'
#' @return Data frame with soil data
#'
#' @examples soil = soil.final.cleanup(soildata)
#' @examples prepare.soil.triangle(soil)
#'
#' @export soil.final.cleanup
#' @export prepare.soil.triangle
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

soil.final.cleanup = function(survey_sums){

  library(stringr)


  soil = survey_sums



  soil$CLAY<-soil$Clay.10.40.cm
  soil$SILT<-soil$Silt.10.40.cm
  soil$SAND<-soil$Sand.10.40.cm




  return(soil)

}




prepare.soil.triangle = function(data, Inftarget = 3.81){
  library(stringr)
  if(any(is.na(data$CLAY))){
    removed = nrow(data[is.na(data$CLAY),])
    warning(paste(removed, "Observations without soil texture data have been removed"))
    data1 = data[complete.cases(data$CLAY),]
  } else {
    print("All observations used")
    data1 = data
  }

  data1$texture = TT.points.in.classes(
    tri.data    = data1,
    class.sys   = "USDA.TT",
    PiC.type    = "t", text.tol=1)

  data1$TextCategory = ifelse(data1$CLAY > 25, "Fine", ifelse(data1$CLAY < 15 & data1$SAND > 80, "Coarse","Coarse"))

  data1$BD_target = replace(data1$BD_target, data1$TextCategory == "Fine", 1.1)
  data1$BD_target = replace(data1$BD_target, data1$TextCategory == "Coarse", 1.4)

  data1$Infilt_target = Inftarget



  data1$Infilt_dist = data1$Infilt_target - data1$Infilt1
  data1$BD_dist = data1$BD_target - data1$Bulk.Density

  data1$Location<-str_sub(data1$Point, -2)

  return(data1)

}
