#' @title Create Species List with common names
#'
#' @description Takes bird data that has been prepared by bird_prepare function and bird_species.common function to create a bird list that can be controlled by transect and surveyyear.
#'
#' @param df A data frame object. Only will take species2 which is created by:  bird_species.common(). See that function for previous steps.
#' @param transect A ranch code or a list of ranch codes ie."TOKA".
#' @param surveyyear A year or multiple years. ie. c(2016,2018)
#'
#' @return A data frame that summerises counts of each bird species per year.
#'
#' @examples data = bird_species.list(species2, "TOKA", 2016)
#'
#' @export bird_species.list
#'

bird_species.list<-function(df,distance =300, transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){

  df2<-bird_species.pointYear(df,distance)
  df3 = subset(df2, Transect %in% transect)
  df4 = subset(df3, YEAR %in% surveyyear)

  species4<-df4 %>%
    dplyr::group_by(Spp) %>%
    dplyr::summarize(Count= sum(COUNT))

  names(species4)<-c("Species Code", "COUNT")
  species4<-arrange(species4, desc(COUNT))

  return(species4)
}
