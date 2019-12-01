#' @title Adds zeros to dataframe for statistical analysis
#'
#' @description Takes bird data that has been prepared by bird_prepare function and turned into species by bird_species.all and merged with visits dataframe created by bird_visits.
#'    The data entered into this function should be created by: pc<-merge(species, visits, by="PointYear", all=TRUE) pc<-droplevels(pc)
#'
#' @param df A data frame object
#'
#' @return A data frame that should be saved as newpc2
#'
#' @examples newpc2 = bird_species.common(pc)
#'
#' @export bird_species.common
#'



add.zeros<-function(sum.data){
  wide <- reshape(pc, v.names="COUNT", idvar="PointYear",timevar="Spp", direction="wide")
  first<-wide[,1:5]
  second<-wide[,6:length(wide[1,])]
  second0 <- second
  second[] <- lapply(second,function(x) replace(x, is.na(x), 0))
  final<-as.data.frame(cbind(first,second))
  narrow<-reshape(final,idvar="PointYear",varying=list(names(final)[6:length(final[1,])]),direction="long",times=names(final)[6:length(final[1,])],v.names="COUNT",timevar="Spp")
  narrow$Spp<-as.factor(substr(narrow$Spp,7,10))
  row.names(narrow)<-NULL
  narrow$ABUNDANCE<-narrow$COUNT/narrow$Visits
  narrow2<-subset(narrow, select=c("PointYear","Transect", "YEAR", "POINT", "Visits", "Spp", "ABUNDANCE"))
  return(narrow2)
}
