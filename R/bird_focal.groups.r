#' @title Create a Focal Species Abundance Graph. Bar for each: Grassland, OakWoodland, and Riparian Spp
#'
#' @description Takes bird data that has been prepared by bird_prepare function, to create a focal species abundance per year graph.
#'
#' @param df2 A data frame object. Only will take bird_prepare(). See those functions for additional previous steps.
#' @param transect A ranch code or a list of ranch codes ie."TOKA".
#' @param distance_focal Filters the data by selecting only detections less than this value.
#' @param surveyyear A year or multiple years. ie. c(2016,2018)
#'
#' @return A graph that summerises focal species abundance.
#'
#' @examples bird_focal.group(df2, transect="TOKA", surveyyear= 2016, distance_focal=300)
#'
#' @export bird_focal.group
#'

bird_focal.groups<-function(df2, distance_focal, transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){

  Grassland<-c("MOPL","SAVS", "GRSP", "WEME","LOSH", "NOHA","FEHA" ,"AMKE","WTKI",  "BUOW" )
  Oak.Woodland<-c("ACWO", "NUWO","NOFL", "ATFL", "WBNU", "WEBL", "OATI", "EUST", "HUVI","BEWR", "LAGO", "BHGR","LASP", "CALT", "CASJ", "YBMA","CAQU" )
  Riparian<-c("ATFL", "NOFL", "NUWO", "LAZB", "BEWR", "SPTO", "YEWA", "COYE", "YBCH", "SOSP", "BHGR", "BLGR", "BUOR", "WAVI")



  data<-subset(df2, subset = df2$Distance.Bin <= distance_focal)
  data$Distance.Bin.ID<-as.factor(data$Distance.Bin.ID)
  df5<-data
  species<-aggregate(df5$Count,list(df5$Spp, df5$Transect, df5$YEAR, df5$Point),sum)
  names(species)<-c("Spp","Transect", "YEAR", "POINT","COUNT")
  species$PointYear<-as.factor(paste(species$POINT,species$YEAR, sep=""))



  visits<-bird_visits(df2, distance =distance_focal )

  pc<-merge(species, visits, by="PointYear", all=TRUE)
  pc2<-droplevels(pc)

  df3<-add.zeros.noCount(pc2)


  df_sub = subset(df3, Transect %in% transect)
  df = subset(df_sub, YEAR %in% surveyyear)



  grass<-df[df$Spp==Grassland,]
  grass2<-grass %>%
    dplyr::group_by(YEAR) %>%
    dplyr::summarise(grassland=sum(ABUNDANCE))
  oak<-df[df$Spp==Oak.Woodland,]
  oak2<-oak %>%
    dplyr::group_by(YEAR) %>%
    dplyr::summarise(oakwoodland=sum(ABUNDANCE))
  rip<-df[df$Spp==Riparian,]
  rip2<-rip %>%
    dplyr::group_by(YEAR) %>%
    dplyr::summarise(riparian=sum(ABUNDANCE))
  focal_join<-left_join(grass2,oak2, by="YEAR")

  focal_join2<- left_join(focal_join, rip2, by="YEAR")


  x<-focal_join2 %>%
    tidyr::gather("new","test", 2:4)
  x$YEAR<-as.factor(x$YEAR)

  titleCustom_focal = paste("Bird Focal Group Abundance per Year: Radius of ", distance_focal, "meters")

  ggplot(x, aes(x=x$new, y=x$test, group=x$YEAR,fill=YEAR))+
    geom_col( position = "dodge")+
    ylab("Total Focal Species Abundance")+
    xlab("Focal Groups")+
    ggtitle(titleCustom_focal)+
    scale_fill_manual(values = c("gray27", "gray55", "gray68", "gray88"))

  }