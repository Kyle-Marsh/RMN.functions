#' @title Create a Focal Species Abundance per Year Graph
#'
#' @description Takes bird data that has been prepared by bird_prepare function, add.zero function and bird_wide.data function to create a focal species per year graph based on ranch transect and focal group
#'
#' @param df A data frame object. Only will take wide.data which is created by: add.zero function , bird_wide.data(), bird_prepare(). See those functions for additional previous steps.
#' @param transect A ranch code or a list of ranch codes ie."TOKA".
#' @param surveyyear A year or multiple years. ie. c(2016,2018)
#' @param choose_focal_group Three bird focal groups: Grassland, Oak Woodland, and Riparian. Each contain Spp codes that we consider focal species for that habitat type.
#'
#' @return A graph that summerises focal species abundance per year.
#'
#' @examples bird_focal.graph(wide.data, transect="TOKA", surveyyear= 2016, choose_focal_group="Grassland")
#'
#' @export bird_focal.graph
#'


bird_focal.graph<-function (df, transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR))), choose_focal_group= c("Grassland", "Oak.Woodland", "Riparian")) {




  Grassland<-c("MOPL","SAVS", "GRSP", "WEME","LOSH", "NOHA","FEHA" ,"AMKE","WTKI",  "BUOW" )
  Oak.Woodland<-c("ACWO", "NUWO","NOFL", "ATFL", "WBNU", "WEBL", "OATI", "EUST", "HUVI","BEWR", "LAGO", "BHGR","LASP", "CALT", "CASJ", "YBMA","CAQU" )
  Riparian<-c("ATFL", "NOFL", "NUWO", "LAZB", "BEWR", "SPTO", "YEWA", "COYE", "YBCH", "SOSP", "BHGR", "BLGR", "BUOR", "WAVI")


  df = subset(df, Transect %in% transect)
  df = subset(df, YEAR %in% surveyyear)


  if(choose_focal_group=="Grassland"){
    df = subset(df, Spp %in% Grassland)
  }
  if(choose_focal_group=="Oak.Woodland"){
    df = subset(df, Spp %in% Oak.Woodland)
  }
  if(choose_focal_group=="Riparian"){
    df = subset(df, Spp %in% Riparian)
  }
  df$YEAR<-as.factor(df$YEAR)
  # df<-df %>%
  #group_by( Spp) %>%
  #tally(ABUNDANCE)

  # df2= df %>%
  #  mutate(average=round(sum(Richness)/length(Richness),0)) %>%
  #mutate(obs_mean2=(Richness-average)^2) %>%
  #mutate(div=obs_mean2/(length(unique(POINT))-1)) %>%
  #mutate(sd=sqrt(div)) %>%
  #mutate(se= sd/(sqrt(length(unique(POINT))-1)))%>%
  #mutate(upper= Richness+se) %>%
  #mutate(lower=Richness-se)

  df2<-df %>%
    dplyr::group_by(Spp) %>%
    dplyr::tally(ABUNDANCE) %>%
    dplyr::filter(n>0)
  keepspp<-as.factor(df2$Spp)
  #gFEHA,GRSP, NOHA, WTKI, WEME, SAVS, LOSH, BUOW


  df<- subset(df, Spp %in% keepspp)



  df2<- df %>%
    dplyr::group_by(Spp,YEAR) %>%
    dplyr::summarise(Abundance=sum(ABUNDANCE))
  df2$average<-mean(df2$Abundance)
  df3<-df2 %>%
    dplyr::mutate(obs_mean2=(Abundance-average)^2)%>%
    dplyr::mutate(div=obs_mean2/(length(unique(df2$Spp))-1))%>%
    dplyr::mutate(sd=sqrt(div)) %>%
    dplyr::mutate(se= sd/(sqrt(length(unique(df2$Spp))-1)))%>%
    dplyr::mutate(upper= Abundance+se) %>%
    dplyr::mutate(lower=Abundance-se)





  ggplot(df3,aes(x=df3$Spp, y=df3$Abundance, group=df3$YEAR, fill=df3$YEAR))+
    geom_col( position = "dodge")+
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) +
    ylab("Focal Species Abundance")+
    xlab("Species Code")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    ggtitle("Bird Focal Species Abundance per Year")+
    scale_fill_manual(values = c("gray27", "gray55", "gray68", "gray88"))




}
