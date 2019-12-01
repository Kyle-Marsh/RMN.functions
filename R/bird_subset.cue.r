#' @title Subset bird data based on detection cue
#'
#' @description Takes bird data and removes observation of flyovers, juveniles, and certain distance bins.
#'
#' @param df A data frame object
#'
#' @return A data frame that is subsetted based on detection
#'
#' @examples data = bird_subset.cue(df)
#'
#' @export bird_subset.cue
#'
#'



bird_subset.cue<-function(df){
  testing<-df %>%
    filter( Distance.Bin.ID != "B30") %>%
    filter(Distance.Bin.ID !="FLO") %>%
    filter(Distance.Bin.ID != "B00") %>%
    filter(Detection.Cue !="J")
  df<-testing
  return(df)
}
