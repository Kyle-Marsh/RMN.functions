#' @title Graph for species abundance by focal group
#'
#' @description Creates a graph of species abundance by focal group
#'
#' @param df A data frame object
#' @param relative boolian: do you want to account for effort
#' @param commonName if you want common names
#' @param distance pick a distance to select by
#' @param surveyyear the year
#' @param transect the transect
#' @param choose_focal_group focal groups
#' @param removeSpecies species you want removed.
#'
#' @return A graph
#'
#' @examples bird_focal.Abundance.Facet(df)
#'
#' @export bird_focal.Abundance.Facet
#'
#'
#'


bird_focal.Abundance.Facet<-function (df,
                                      relative= T,
                                      commonName=TRUE,
                                      distance,
                                      transect=c(levels(as.factor(df$Transect))),
                                      surveyyear=c(levels(as.factor(df$YEAR))),
                                      choose_focal_group= c("Grassland", "Oak.Woodland", "Riparian", "Mountian.Meadow"),
                                      removeSpecies= c()) {


  NumberOfPoints <- numberOfPoints(df,common = commonName)

  dataPrepared<-prepareData_R_Abundance(df,NumberOfPoints = NumberOfPoints, common = commonName, distance = distance)

  dataSubseted = subset(dataPrepared, Transect %in% transect)
  dataSubseted = subset(dataSubseted, YEAR %in% surveyyear)
  dataSubseted = subset(dataSubseted, Spp %in% focalGroupArrayCN[[choose_focal_group]])
  dataSubseted = subset(dataSubseted, !(Spp %in% removeSpecies))


  dataSubseted$YEAR<-as.factor(dataSubseted$YEAR)
  if(relative){
    titleCustom = paste(choose_focal_group ,": Bird Species Relative Abundance: Radius of " , distance, "meters")


    ggplot(dataSubseted,aes(x=as.factor(YEAR), y=Relative.Abundance, fill = YEAR))+
      geom_col( position = "dodge")+
      facet_wrap( ~ dataSubseted$Spp)+
      ylim(0,max(dataSubseted$Relative.Abundance)+(.25*max(dataSubseted$Relative.Abundance)))+
      ylab("Focal Species Abundance")+
      xlab("Year")+
      #theme(axis.text.x = element_text(angle = 40, hjust = 1))+
      #geom_text(aes(label=as.character(derp2$Relative.Abundance)), position=position_dodge(width=0.9), vjust=-0.25)+
      #geom_text(aes(label=as.character(df3$Abundance)),vjust=-.7 )+
      ggtitle(titleCustom)+
      scale_fill_manual(values = c("olivedrab", "olivedrab3", "gray38", "gray12"))

  }else if( relative ==FALSE){
    titleCustom = paste(choose_focal_group, ": Bird Species Total Abundance: Radius of " , distance, "meters")


    ggplot(dataSubseted,aes(x=as.factor(YEAR), y=Abundance, fill = YEAR))+
      geom_col( position = "dodge")+
      facet_wrap( ~ dataSubseted$Spp)+
      ylim(0,max(dataSubseted$Abundance)+ (.1*max(dataSubseted$Abundance)))+
      ylab("Focal Species Abundance")+
      xlab("Year")+
      #theme(axis.text.x = element_text(angle = 40, hjust = 1))+
      #geom_text(aes(label=as.character(df5$Abundance)), position=position_dodge(width=0.9), vjust=-0.25)+
      #geom_text(aes(label=as.character(df3$Abundance)),vjust=-.7 )+
      ggtitle(titleCustom)+
      scale_fill_manual(values = c("olivedrab", "olivedrab3", "gray38", "gray12"))


  }

}
