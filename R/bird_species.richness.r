#' @title Create a Species Richness per Point Graph
#'
#' @description Takes bird data that has been prepared by bird_prepare function and add.zero function to create a species richness per point graph based on ranch transect and surveyyear.
#'
#' @param df A data frame object. Only will take newpc2 which is created by:  add.zero() , bird_prepare(). See those functions for additional previous steps.
#' @param transect A ranch code or a list of ranch codes ie."TOKA".
#' @param surveyyear A year or multiple years. ie. c(2016,2018)
#' @param relative if you want to have the graph consider effort.
#' @param distance distance.

#'
#' @return A graph that summerises species richness of birds per point and year.
#'
#' @examples bird_species.richness(newpc2, "TOKA", 2016)
#'
#' @export bird_species.richness
#'



bird_species.richness<-function(df,
                                relative = T,
                                distance = 300,
                                transect=c(levels(as.factor(df$Transect))),
                                surveyyear=c(levels(as.factor(df$YEAR)))){


  df5<-prepareBirdRichnessData(df,
                               distance = distance,
                               transect = transect,
                               surveyyear = surveyyear)

  if(relative){
    titleCustom = paste("Bird Species Relative Richness per Point: Radius of " , distance, "meters")


    ggplot(df5,aes(x=df5$POINT, y=as.factor(df5$RelativeRichness), fill=as.factor(df5$YEAR), group=YEAR))+
      geom_col( position = "dodge")+
      ylab("Bird Species Relative Richness")+
      xlab("Point Id")+
      theme(axis.text.x = element_text(angle = 40, hjust = 1))+
      geom_text(aes(label=as.character(df5$RelativeRichness)), position=position_dodge(width=0.9), vjust=-0.25)+
      ggtitle(titleCustom)+
      scale_fill_manual(name ="Year",values = c("gray28","dodgerblue4","deepskyblue3","lightblue3", "lightblue4", "lightblue3","lightblue1"))


  }else if (relative== FALSE){
    titleCustom = paste("Bird Species Richness per Point: Radius of " , distance, "meters")


    ggplot(df5,aes(x=df5$POINT, y=as.factor(df5$Richness), fill=as.factor(df5$YEAR), group=YEAR))+
      geom_col( position = "dodge")+
      ylab("Bird Species Total Richness")+
      xlab("Point Id")+
      theme(axis.text.x = element_text(angle = 40, hjust = 1))+
      geom_text(aes(label=as.character(df5$Richness)), position=position_dodge(width=0.9), vjust=-0.25)+
      ggtitle(titleCustom)+
      scale_fill_manual(name ="Year",values = c("gray28","dodgerblue4","deepskyblue3","lightblue3", "lightblue4", "lightblue3","lightblue1"))

  }

}
