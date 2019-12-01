#' @title Create a table of cover for functional groups at each point
#'
#' @description For veg data, summarizes cover of each functional group at each point.
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param transect Ranch for which to make cover table
#' @param type Type of cover to calculate - either "absolute" or "relative"
#' @param invasives Boolean that specifies whether to count invasive species as a separate functional group
#' @param surveyyear Year for which to calculate cover
#' @param includemeta =FALSE Boolean on whether to include columns specifying Transect and pointyear. Useful if using for further analysis but unecessary for display
#'
#' @return A dataframe summary of cover of functional groups
#'
#' @examples data = functional.cover.table(lpi, type = "absolute")
#'
#' @export functional.cover.table
#'
#'
#'


functional.cover.table = function(lpi,
               transect,
               type = "absolute",
               invasives = FALSE,
               surveyyear = max(levels(as.factor(lpi$year))),
               includemeta = FALSE){

  library(reshape2)
  CAPlants = RMN:::CAPlantsv2
  Invasives = RMN:::Invasivesv1



  CAPlantsI<-CAPlants
  levels(CAPlantsI$FunGrp) <- c(levels(CAPlantsI$FunGrp), "Invasives")

  CAPlantsI$FunGrp[CAPlantsI$Accepted.Symbol %in% Invasives$USDA.code]<-"Invasives"
  lpi$year = as.factor(lpi$year)
  lpi = subset(lpi, subset = lpi$year %in% surveyyear)
  lpi = subset(lpi, subset = lpi$Transect.Name %in% transect)
  lpi$Tally = 1

  a = aggregate(lpi$Tally, list(lpi$pointyear), sum)
  names(a) = c("pointyear", "NumIndices")

  lpi.trim = subset(lpi, select=c("Transect.Name", "pointyear", "year", "Point.Id", "Canopy1",
                                  "Canopy2", "Canopy3","Top.Layer","Lower1","Lower2", "Lower3",
                                  "Lower4","Lower5","Lower6","Lower7",
                                  "Lower8","Lower9","Lower10","Soil.Surface"))

  longlpi = melt(lpi.trim, id=c("Transect.Name", "pointyear", "Point.Id", "year"))
  # Note, this step may give a warning, but it's
  names(longlpi) = c("Transect.Name", "pointyear", "Point.Id", "year", "Layer", "Spp")

  longlpi = subset(longlpi, select=c("Transect.Name", "pointyear", "Point.Id", "year", "Spp"))
  longlpi$Tally = 1



  longlpi$FunGrp = NULL
  if(isTRUE(invasives)){
    longlpi$FunGrp<-CAPlantsI$FunGrp[match(longlpi$Spp,CAPlantsI$Accepted.Symbol)]
  } else if(isFALSE(invasives)){
    longlpi$FunGrp = CAPlants$FunGrp[match(longlpi$Spp,CAPlants$Accepted.Symbol)]
  } else {stop("Argument for invasives not recognized")}


  Fun.Sum = aggregate(longlpi$Tally, by = list(longlpi$Transect.Name, longlpi$pointyear, longlpi$Point.Id, longlpi$year, longlpi$FunGrp), sum)
  names(Fun.Sum) = c("Transect" ,"pointyear", "Point.Id", "year", "FunGrp", "Count")


  Fun.Sum2 = dcast(Fun.Sum, pointyear ~ FunGrp, value.var = c("Count"), sum)
  Fun.Sum2 = merge(Fun.Sum2, a, by = "pointyear")
  pointyears = as.data.frame(Fun.Sum[,1:4])
  pointyears = unique(pointyears)





  Fun.Sum3 = Fun.Sum2
  if(type == "absolute"){
    Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)] = Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)]/Fun.Sum3[,ncol(Fun.Sum3)] * 100
  } else if(type == "relative"){
    Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)] = Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)]/rowSums(Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)], na.rm = TRUE) * 100
  } else {stop("Argument for type not recognized")}

  Fun.Sum3 = merge(pointyears, Fun.Sum3, by = "pointyear", all = FALSE)

  if(includemeta){
    return(Fun.Sum3)
  }else{
    Fun.Sum3$pointyear = NULL
    Fun.Sum3$Transect = NULL
    Fun.Sum3[,3:ncol(Fun.Sum3)] = round(Fun.Sum3[,5:ncol(Fun.Sum3)], 2)
    return(Fun.Sum3)
  }


}
