#' @title Prepared data for by getting number of points
#'
#' @description  This gets the amount of effort per ranch per year
#'
#' @param df is a dataframe. Only works with newpc2 created by add.zeros() and it's previous steps.
#' @param common This defines if you want to add in common names of birds to the dataframe.
#'
#' @return Data frame with effort per ranch per year
#'
#' @examples numberOfPoints(df, common=T)
#'
#' @export numberOfPoints
#'
#'
#'
numberOfPoints<- function(df, common){
  if(common ==TRUE){
    species<- bird_species.pointYear(df, 300)
    pc<-droplevels(species)
    newpc2<-add.zeros.commonName(pc)
  }else if (common==FALSE){
    species<-bird_species.narrow(df, 300)
    pc<-droplevels(species)
    newpc2<-add.zeros.noCount(pc)
  }
  points.variable =newpc2 %>%
    dplyr::group_by(Transect, YEAR) %>%
    dplyr::distinct(POINT) %>%
    mutate(pointyear=paste(POINT,YEAR, sep = "_"))

  NumberOfPoints<-points.variable %>%
    dplyr::group_by(Transect,YEAR) %>%
    dplyr::summarize(NumberOfPointCountLocations=length(pointyear)) %>%
    dplyr::mutate(tranYear=paste(Transect,YEAR,sep="_")) %>%
    select("tranYear", "NumberOfPointCountLocations")
  return(NumberOfPoints)
}
