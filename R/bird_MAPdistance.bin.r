#' @title Replaces Distance Bin Values from different RMN point count protocols.
#'
#' @description Replaces distance bin Values from character strings to number values. There are four different RMN point count protocols with some resulting in numeric numbers and some wiht character strings (10-20). When analyzing data from multiple protocols you need to account for these differences.
#'
#' @param df A dataframe object
#'
#' @return A dataframe with the column Distance.bin as numeric values
#'
#' @examples data = bird_MAPdistance.bin(df)
#'
#' @export bird_MAPdistance.bin
#'
#'
#'
#'











bird_MAPdistance.bin<-function (df){
  if(is.factor(df$Distance.Bin)){
    df$Distance.Bin <- mapvalues(df$Distance.Bin, c("<0","75 to 100", "50 to 75", ">75", ">50","<50" ,"", "<10", "10 to 20", "20 to 30", "30 to 50", "30 to 40" ,"40 to 50", "50 to 100", ">100" ,"FlyOver", ">300"), c(0,85, 65, 300, 65, 25, 300,5, 15, 25, 35 ,40,45 ,  75, 300, 300,300))
    df$Distance.Bin<-as.numeric(as.character(df$Distance.Bin))
    return(df)
  }else if(is.numeric(df$Distance.Bin) ){ return(df) }}
