#' @title Prepared data for visualization
#'
#' @description  This function turns the dataframe that prepares the data for analysis and graphs
#'
#' @param df is a dataframe. Only works with newpc2 created by add.zeros() and it's previous steps.
#' @param distance is a dataframe. Only works with newpc2 created by add.zeros() and it's previous steps.
#' @param transect is a dataframe. Only works with newpc2 created by add.zeros() and it's previous steps.
#' @param surveyyear is a dataframe. Only works with newpc2 created by add.zeros() and it's previous steps.
#'
#' @return Data frame with the is the wide version of newpc2 with added column of Richness
#'
#' @examples prepareBirdRichnessData(df)
#'
#' @export prepareBirdRichnessData
#'
#'
#'




prepareBirdRichnessData<-function(df,distance, transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){
  a<-subset(df,duplicated(df$SURVEY)==FALSE)
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

  df5<-df5[,c("YEAR", "POINT","Visits", "Richness")]
  df5<-df5 %>%
    mutate(RelativeRichness= Richness/Visits)

  df5[,2]<-as.factor(df5[,2])
  return(df5)
}
