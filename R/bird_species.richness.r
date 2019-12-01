#' @title Create a Species Richness per Point Graph
#'
#' @description Takes bird data that has been prepared by bird_prepare function and add.zero function to create a species richness per point graph based on ranch transect and surveyyear.
#'
#' @param df A data frame object. Only will take newpc2 which is created by:  add.zero() , bird_prepare(). See those functions for additional previous steps.
#' @param transect A ranch code or a list of ranch codes ie."TOKA".
#' @param surveyyear A year or multiple years. ie. c(2016,2018)
#'
#' @return A graph that summerises species richness of each bird species per point and year.
#'
#' @examples data = bird_species.richness(newpc2, "TOKA", 2016)
#'
#' @export bird_species.list
#'



bird_species.richness<-function(df,transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){


  df$YEAR<-as.factor(df$YEAR)

  df = subset(df, Transect %in% transect)
  df = subset(df, YEAR %in% surveyyear)

  df2<-df[,c("YEAR", "POINT", "Richness")]
  df2$average=round(sum(df2$Richness)/length(df2$Richness),0)
  df2$obs_mean2=((df2$Richness-df2$average)^2)
  df2$div=(df2$obs_mean2/(length(unique(df2$POINT))-1))
  df2$sd=sqrt(df2$div)
  df2$se= df2$sd/(sqrt(length(unique(df2$POINT))-1))
  df2$upper= (df2$Richness+df2$se)
  df2$lower=(df2$Richness-df2$se)

  ggplot(df2,aes(x=df2$POINT, y=df2$Richness,fill=YEAR, group=df2$YEAR))+
    geom_col( position = "dodge")+
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) +
    ylab("Bird Species Richness")+
    xlab("Point Id")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    ggtitle("Bird Species Richness per Point")+
    scale_fill_manual(values = c("gray28","dodgerblue4","deepskyblue3","lightblue3", "lightblue4", "lightblue3","lightblue1"))



}
