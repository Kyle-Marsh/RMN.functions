#' @title Create cover summary table for veg data
#'
#' @description For veg data, summarizes cover of shrubs, trees, litter, thatch, bare ground. Tree and shrub covers come from releve estimates
#' @description NOTE: Thatch was only measured starting in 2017.
#'
#' @param releve A dataframe object of releve data from a veg survey
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param transect The ranch for which to make the summary
#' @param surveyyear The years for which to make the summary
#' @param choose.variable Character vector that identifies which variables to summarize. Defaults to Species Richness, Litter, Thatch, Bare Ground, Trees, and Shrubs
#' @param rounded Boolean whether to round values in table
#'
#' @return A summary of cover
#'
#' @examples data = cover.summary(releve, lpi)
#'
#' @export cover.summary
#'
#'


cover.summary = function(lpi, releve,
                         transect,
                         surveyyear = c(levels(as.factor(lpi$year)), levels(as.factor(releve$year))),
                         choose.variable = c("SpeciesRichness", "Litter", "Thatch",
                                             "BareGround", "Trees", "Shrubs"),
                         rounded = TRUE){

  library(reshape2)
  library(ggplot2)
  library(dplyr)


  lpi$year = as.factor(lpi$year)
  releve$year = as.factor(releve$year)
  lpi = subset(lpi, Transect.Name %in% transect)
  releve = subset(releve, Transect.Name %in% transect)
  releve = subset(releve, year %in% surveyyear)
  lpi = subset(lpi, year %in% surveyyear)

  covsum<- ddply(releve, .(Vegetation.Type, pointyear), summarise, Percent.Cover=sum(Percent.Cover), .drop=F)
  shrubs<-subset(covsum, subset=covsum$Vegetation.Type == "shrubs")
  colnames(shrubs) = c("covertype", "pointyear", "Shrubcover")
  shrubs = subset(shrubs, select = c("pointyear", "Shrubcover"))
  trees<-subset(covsum, subset=covsum$Vegetation.Type == "trees")
  colnames(trees) = c("covertype", "pointyear", "Treecover")
  trees = subset(trees, select = c("pointyear", "Treecover"))

  lpi$BG<-0
  lpi$BG <- replace(lpi$BG,
                    lpi$Top.Layer == "NOPLANT" &
                      lpi$Lower1 == "" & lpi$Lower2 == "" & lpi$Lower3 == "" & lpi$Soil.Surface == "S", 1)

  #Aggregate Bare Grounds By Point
  BareGround<-aggregate(lpi$BG,list(lpi$pointyear),sum)
  names(BareGround)<-c("pointyear", "BareGround")


  #Calculate Litter
  #First, rename all of the things that could be litter into just one term
  lpi$Litter<-0
  lpi$Litter <- replace(lpi$Litter, lpi$Lower1 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower2 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower3 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower4 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower5 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower6 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower7 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower8 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower9 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower10 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Soil.Surface == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Soil.Surface == "EM", 1)

  Litter<-aggregate(lpi$Litter,list(lpi$pointyear),sum)
  names(Litter)<-c("pointyear", "Litter")

  lpi$Thatch<-1
  lpi$Thatch<-replace(lpi$Thatch, is.na(lpi$Thatch.Indices.Lower), 0)
  lpi$Thatch<-replace(lpi$Thatch, lpi$Thatch.Indices.Lower == "", 0)
  lpi$Thatch<-replace(lpi$Thatch, is.na(lpi$Thatch.Top.Layer), 0)
  lpi$Thatch<-replace(lpi$Thatch, lpi$Thatch.Top.Layer == "", 0)

  Thatch<-aggregate(lpi$Thatch,list(lpi$pointyear),sum, na.rm = TRUE)
  names(Thatch)<-c("pointyear", "Thatch")


  lpi$Point.Dir<-paste(lpi$pointyear, lpi$Direction, sep="-")

  layers<-subset(lpi, select=c("pointyear",  "Top.Layer", "Lower1", "Lower2",
                               "Lower3", "Lower4", "Lower5", "Lower6", "Lower7"))

  longlpi<-melt(layers, id="pointyear")
  names(longlpi)<-c("pointyear", "Layer", "Spp")
  releve$Layer<-"extras"
  extras<-subset(releve, select=c("pointyear", "Layer", "USDA.Code"))
  names(extras)<-c("pointyear", "Layer", "Spp")

  both<-rbind(longlpi, extras)
  both$Spp<-as.factor(both$Spp)

  #Remove the non-spp
  Exclude<-c("", "2FA", "2FORB", "2FP", "2GA", "2GP", "2LICHN",
             "2LTR", "2LTRWS",  "2PLANT",  "2W", "NOPLANT", "L", "WL")
  both <- both[!(both$Spp %in% Exclude),]
  both<-droplevels(both)


  ##add plants that are in the soil surface hit to this that##
  CAPlants = RMN:::CAPlantsv2
  richness<-NULL
  both1 = both
  both1$Layer = NULL
  both1 = both1[!duplicated(both1),]
  richness = both1
  richness<-merge(richness, CAPlants, by.x = "Spp", by.y = "Accepted.Symbol", all.x = TRUE, all.y = FALSE)
  richness2<-aggregate(richness$Spp, list(richness$pointyear), length)
  names(richness2)<-c("pointyear", "NumSpp")


  #### Now bring everything together

  Pointyears = subset(main, select = c(pointyear, PointId, year))
  data.summary = Pointyears
  data.summary = merge(data.summary, richness2, by = "pointyear", all.x=TRUE)
  data.summary<-merge(data.summary, Litter, by="pointyear", all.x=TRUE)
  data.summary<-merge(data.summary, Thatch,by="pointyear", all.x=TRUE)
  data.summary<-merge(data.summary, BareGround,by="pointyear", all.x=TRUE)
  data.summary<-merge(data.summary, trees,by="pointyear", all.x=TRUE)
  data.summary<-merge(data.summary, shrubs,by="pointyear", all.x=TRUE)


  # IF you have transect where Not every point has all 100 subsamples,
  # Then run this to correct for reduced effort

  lpi$Tally<-1
  indices<-aggregate(lpi$Tally, list(lpi$pointyear), sum)
  names(indices)<-c("pointyear", "NumIndices")

  data.summary<-merge(data.summary, indices, by="pointyear")
  data.summary$Litter<-(data.summary$Litter/data.summary$NumIndices)*100
  data.summary$BareGround<-(data.summary$BareGround/data.summary$NumIndices)*100
  data.summary$Thatch = (data.summary$Thatch/data.summary$NumIndices)*100

  #select variables you want to
  if(!("SpeciesRichness" %in% choose.variable)){data.summary$NumSpp = NULL}
  if(!("Litter" %in% choose.variable)){data.summary$Litter = NULL}
  if(!("Thatch" %in% choose.variable)){data.summary$Thatch = NULL}
  if(!("BareGround" %in% choose.variable)){data.summary$BareGround = NULL}
  if(!("Trees" %in% choose.variable)){data.summary$Treecover = NULL}
  if(!("Shrubs" %in% choose.variable)){data.summary$Shrubcover = NULL}

  if(rounded){data.summary[,5:ncol(data.summary)] = round(data.summary[,5:ncol(data.summary)], 2)}


  return(data.summary)

}


