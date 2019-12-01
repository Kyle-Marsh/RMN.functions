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

bird_birdcount.year<-function(df,transect, surveyyear=c(levels(as.factor(df$YEAR)))){


  df = subset(df, Transect %in% transect)
  df = subset(df, YEAR %in% surveyyear)

  species2<-df %>%
    arrange(desc(COUNT)) %>%
    select(1:6)
  df<-species2[,c(1,5,4,3,2,6)]

  return(df)
}
