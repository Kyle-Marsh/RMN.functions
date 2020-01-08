#' @title Create species list for veg data
#'
#' @description For veg data, summarizes cover of shrubs, trees, litter, thatch, bare ground. Tree and shrub covers come from releve estimates
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param releve A dataframe object of releve data from a veg survey
#' @param transect Ranch for which to generate species list
#' @param surveyyear Year for which to generate list
#'
#' @return A species list for a given dataset
#'
#' @examples data = species.list(lpi, checklist)
#'
#' @export species.list
#'
#'

species.list = function(lpi, releve,
                        transect,
                        surveyyear = levels(as.factor(lpi$year))){
  library(reshape2)
  CAPlants = RMN:::CAPlantsv2
  Invasives = RMN:::Invasivesv1

  lpi$Point.Dir = paste(lpi$pointyear, lpi$Direction, sep = "-")
  lpi$year = as.factor(lpi$year)
  releve$year = as.factor(releve$year)

  lpi = subset(lpi, subset = lpi$year %in% surveyyear)
  releve = subset(releve, subset = releve$year %in% surveyyear)

  lpi = subset(lpi, subset = Transect.Name %in% transect)
  releve = subset(releve, subset = Transect.Name %in% transect)

  layers = subset(lpi, select=c("pointyear",  "Top.Layer", "Lower1", "Lower2",
                               "Lower3", "Lower4", "Lower5", "Lower6", "Lower7",
                               "Lower8", "Lower9", "Soil.Surface"))

  longlpi = melt(layers, id="pointyear")
  names(longlpi) = c("pointyear", "Layer", "Spp")
  releve$Layer = "extras"
  extras = subset(releve, select=c("pointyear", "Layer", "USDA.Code"))
  names(extras) = c("pointyear", "Layer", "Spp")

  both = rbind(longlpi, extras)
  both$Spp = as.factor(both$Spp)

  #Remove the non-spp
  Exclude = c("", "2FA", "2FORB", "2FP", "2GA", "2GP", "2LICHN",
             "2LTR", "2LTRWS",  "2PLANT",  "2W", "NOPLANT", "L", "WL")
  both = both[!(both$Spp %in% Exclude),]
  both = droplevels(both)


  ##add plants that are in the soil surface hit to this that##

  richness = NULL
  both1 = both
  both1$Layer = NULL
  both1 = both1[!duplicated(both1),]
  richness = both1
  richness = merge(richness, CAPlants, by.x = "Spp", by.y = "Accepted.Symbol", all.x = TRUE, all.y = FALSE)
  richness2 = aggregate(richness$Spp, list(richness$pointyear), length)
  names(richness2) = c("pointyear", "NumSpp")

  SppList = subset(richness, select=c("Spp", "Symbol", "Scientific.Name", "Common.Name","Genus","Family","Family.Common.Name",
                                     "Duration","Growth.Habit", "Native.Status" ,"Invasive", "FunGrp"))

  SppList = subset(SppList, !duplicated(SppList$Spp))

  levels(CAPlants$FunGrp) = c(levels(CAPlants$FunGrp), "Invasives")

  CAPlants$FunGrp[CAPlants$Accepted.Symbol %in% Invasives$USDA.code] = "Invasives"

  #########################################################
  ######

  SppList$Native.Status = substr(SppList$Native.Status, 1, 7)

  ##### Make a Native/Invasive category
  SppList$Provenance = NULL
  SppList$Provenance = replace(SppList$Provenance, SppList$Native.Status == "L48 (N)", "Native")
  SppList$Provenance = replace(SppList$Provenance, SppList$Native.Status == "L48 (I)", "Non-native")

  SppList2 = subset(SppList, select=c("Symbol", "Scientific.Name", "Common.Name", "Family", "Provenance", "FunGrp"))


  return(SppList2)

}
