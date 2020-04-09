#' @title prepare data with common names
#'
#' @description Creates a dataframe
#'
#' @param df A data frame object
#'
#' @return A df
#'
#' @examples add.zeros.commonName(df)
#'
#' @export add.zeros.commonName
#'





add.zeros.commonName<-function(pc2){

  columnsRemoved<-names(pc2) %in% c("Spp","Scientific.Name")
  pc2_reduced<-pc2[!columnsRemoved]
  wide <- reshape(pc2_reduced, v.names="COUNT", idvar="PointYear",timevar="Common.Name", direction="wide")
  first<-wide[,1:5]
  second<-wide[,6:length(wide[1,])]
  second0 <- second
  second[] <- lapply(second,function(x) replace(x, is.na(x), 0))
  final<-as.data.frame(cbind(first,second))
  narrow<-reshape(final,idvar="PointYear",varying=list(names(final)[6:length(final[1,])]),direction="long",times=names(final)[6:length(final[1,])],v.names="COUNT",timevar="Spp")
  narrow2<-separate(data = narrow, col = Spp, into = c("Count", "Spp"), sep = "\\.")
  countRemoved<-names(narrow2)%in%c("Count")
  narrow2<-narrow2[!countRemoved]
  row.names(narrow2)<-NULL
  narrow2$ABUNDANCE<-narrow2$COUNT/narrow2$Visits
  narrow2<-subset(narrow2, select=c("PointYear","Transect", "YEAR", "POINT", "Visits", "Spp", "ABUNDANCE"))
  return(narrow2)
}
