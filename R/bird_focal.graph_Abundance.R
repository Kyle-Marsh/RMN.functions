#' @title Graph for species abundance by focal group
#'
#' @description Creates a graph of species abundance by focal group
#'
#' @param df A data frame object
#' @param relative boolian: do you want to account for effort
#' @param common if you want common names
#' @param distance pick a distance to select by
#' @param transect the transect
#' @param surveyyear the year
#' @param choose_focal_group focal groups
#'
#' @return A graph
#'
#' @examples bird_focal.graph_Abundance(df)
#'
#' @export bird_focal.graph_Abundance
#'

bird_focal.graph_Abundance=function(df,relative, commonName = FALSE, distance=300,transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR))), choose_focal_group= c("Grassland", "Oak.Woodland", "Riparian", "Mountian.Meadow")){

  NumberOfPoints<-numberOfPoints(df, common = commonName)

  df2<-prepareData_R_Abundance(df,NumberOfPoints = NumberOfPoints, common = commonName, distance = distance)

  df2 = subset(df2, Transect %in% transect)
  df2 = subset(df2, YEAR %in% surveyyear)
  df2 = subset(df2, Spp %in% focalGroupArray[[choose_focal_group]])

  df2$YEAR<-as.factor(df2$YEAR)
  if(relative){
    titleCustom = paste(choose_focal_group ,": Bird Species Relative Abundance: Radius of " , distance, "meters")

    ggplot(df2,aes(x=df2$Spp, y=df2$Relative.Abundance, group=df2$YEAR, fill=YEAR))+
      geom_col( position = "dodge")+
      ylab("Focal Species Abundance")+
      xlab("Species Code")+
      theme(axis.text.x = element_text(angle = 40, hjust = 1))+
      geom_text(aes(label=as.character(df2$Relative.Abundance)), position=position_dodge(width=0.9), vjust=-0.25)+
      #geom_text(aes(label=as.character(df3$Abundance)),vjust=-.7 )+
      ggtitle(titleCustom)+
      scale_fill_manual(values = c("olivedrab", "olivedrab3", "gray38", "gray12"))
  } else if(relative==F){
    titleCustom = paste(choose_focal_group ,": Bird Species Total Abundance: Radius of " , distance, "meters")

    ggplot(df2,aes(x=df2$Spp, y=df2$Abundance, group=df2$YEAR, fill=YEAR))+
      geom_col( position = "dodge")+
      ylab("Focal Species Abundance")+
      xlab("Species Code")+
      theme(axis.text.x = element_text(angle = 40, hjust = 1))+
      geom_text(aes(label=as.character(df2$Abundance)), position=position_dodge(width=0.9), vjust=-0.25)+
      #geom_text(aes(label=as.character(df3$Abundance)),vjust=-.7 )+
      ggtitle(titleCustom)+
      scale_fill_manual(values = c("olivedrab", "olivedrab3", "gray38", "gray12"))
  }

}
