#' @title Summarize data for analysis
#'
#' @description Takes data and combines with numberOfPoints to get the right effort.
#'
#' @param df A data frame object
#' @param NumberOfPoints A data frame object
#' @param common if you want common names
#' @param distance pick a distance to select by
#'
#' @return A data frame with visits
#'
#' @examples data = prepareData_R_Abundance(df)
#'
#' @export prepareData_R_Abundance
#'



prepareData_R_Abundance<-function(df, NumberOfPoints= NumberOfPoints, common , distance){
  if(common){
    narrow<-bird_species.pointYear(df, distance = distance)
    pc2<-droplevels(narrow)
    newpc2<-add.zeros.commonName(pc2)
  }else{
    narrow<-bird_species.narrow(df, distance = distance)
    pc2<-droplevels(narrow)
    newpc2<-add.zeros.noCount(pc2)
  }

  df2<- newpc2 %>%
    dplyr::mutate(tranYear=paste(Transect,YEAR,sep = "_")) %>%
    dplyr::group_by(Spp,tranYear,YEAR) %>%
    dplyr::summarise(Abundance=sum(ABUNDANCE)) %>%
    merge(NumberOfPoints,by="tranYear") %>%
    dplyr::mutate(Relative.Abundance=round(Abundance/NumberOfPointCountLocations, digits=2))
  return(df2)
}
