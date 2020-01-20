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




bird_species.richness<-function(df,distance = 300, transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){

  data<-subset(df, subset = df$Distance.Bin <= distance)
  data$Distance.Bin.ID<-as.factor(data$Distance.Bin.ID)
  data = subset(data, Transect %in% transect)
  data = subset(data, YEAR %in% surveyyear)
  df1<-data
  a<-subset(df1,duplicated(df1$SURVEY)==FALSE)
  visits<-aggregate(a$Tally, list(a$PointYear), sum)
  names(visits)<-c("PointYear", "Visits")


  data2<-subset(df, subset = df$Distance.Bin <= distance)
  data2$Distance.Bin.ID<-as.factor(data2$Distance.Bin.ID)
  data2 = subset(data2, Transect %in% transect)
  data2 = subset(data2, YEAR %in% surveyyear)
  df2<-data2

  species2<-aggregate(df2$Count,list(df2$Spp , df2$Transect, df2$YEAR, df2$Point),sum)
  names(species2)<-c("Spp", "Transect", "YEAR", "POINT","COUNT")
  species2$PointYear<-as.factor(paste(species2$POINT,species2$YEAR, sep=""))

  species<-left_join(species2, visits, by="PointYear")
  df3<-add.zeros.noCount(species)

  df4<-reshape(df3, v.names="ABUNDANCE", idvar="PointYear",timevar="Spp", direction="wide")

  JustSpp<-substr(names(df4[,6:ncol(df4)]),11,14)
  colnames(df4)[6:ncol(df4)] <- JustSpp

  first<-df4[,1:5]
  second<-df4[,6:length(df4[1,])]
  second<-second[,order(colnames(second))]
  df5<-as.data.frame(cbind(first,second))


  df5$Richness<-rowSums(df5[,5:ncol(df5)] != 0)




  df5<-df5[,c("YEAR", "POINT", "Richness")]
  df5[,2]<-as.factor(df5[,2])

  titleCustom = paste("Bird Species Richness per Point: Radius of " , distance, "meters")

  ggplot(df5,aes(x=df5$POINT, y=as.factor(df5$Richness), fill=as.factor(df5$YEAR), group=YEAR))+
    geom_col( position = "dodge")+
    ylab("Bird Species Richness")+
    xlab("Point Id")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    geom_text(aes(label=as.character(df5$Richness)), position=position_dodge(width=0.9), vjust=-0.25)+
    ggtitle(titleCustom)+
    scale_fill_manual(name ="Year",values = c("gray28","dodgerblue4","deepskyblue3","lightblue3", "lightblue4", "lightblue3","lightblue1"))



    }
