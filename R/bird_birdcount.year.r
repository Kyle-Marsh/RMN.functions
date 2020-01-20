#' @title Create Species List
#'
#' @description Takes bird data that has been prepared by bird_prepare function and bird_species.common function to create a bird list that can be controlled by transect and surveyyear.
#'
#' @param df A data frame object. Only will take species which is created by:  bird_species.common(). See that function for previous steps.
#' @param transect A ranch code or a list of ranch codes ie."TOKA".
#' @param surveyyear A year or multiple years. ie. c(2016,2018)
#'
#' @return A data frame that summerises counts of each bird species per year.
#'
#' @examples data = bird_birdcount.year(species, "TOKA", 2016)
#'
#' @export bird_birdcount.year
#'
#'


bird_birdcount.year<-function(df, distance =300, transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){


  visits<-bird_visits(df)

  data<-subset(df, subset = df$Distance.Bin < distance)
  data$Distance.Bin.ID<-as.factor(data$Distance.Bin.ID)
  data = subset(data, Transect %in% transect)
  data = subset(data, YEAR %in% surveyyear)
  df<-data


  species2<-aggregate(df$Count,list(df$Spp,df$Common.Name ,df$Scientific.Name  , df$Transect, df$YEAR),sum)
  names(species2)<-c("Spp","Common.Name", "Scientific.Name", "Transect", "YEAR","COUNT")


  df4<- species2%>%
    dplyr::group_by(Spp, Transect, YEAR ) %>%
    dplyr::arrange(desc(COUNT))

#  species<-left_join(df2, df3, by="Spp")

  return(df4)
}