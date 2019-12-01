#' @title Convert distance bins into continuous values
#'
#' @description The Rangeland Monitoring Program has a few different methods for collecting bird point count data: VCP300, VCP10_30, VCP25, and FR50. To analyze these datasets together the distance bin values (aka character strings) need to be converted to numbers (integers). This is useful for data collected using VCP10_30, VCP25, or FR50 protocols.
#'This was done using the mapvalues function and assigning a number value to all bins. I averaged the max and min values for each distance bin to determine the number value. This should only be used to analyzing point count data as presence/absence and should not be used for density analysis.
#'
#' @param df A dataframe object
#'
#' @return Point Count data with distance.bin column as numeric.
#'
#' @examples data = bird_convert.distance(data)
#'
#' @export remove_first
#'
#'
#'
#'
#'


bird_convert.distance<-function (df){
  if(is.factor(df$Distance.Bin)){
    df$Distance.Bin <- mapvalues(df$Distance.Bin, c("75 to 100", "50 to 75", ">75", ">50","<50" ,"", "<10", "10 to 20", "20 to 30", "30 to 50", "30 to 40" ,"40 to 50", "50 to 100", ">100" ,"FlyOver", ">300"), c(85, 65, 300, 65, 25, 300,5, 15, 25, 35 ,40,45 ,  75, 300, 300,300))
    df$Distance.Bin<-as.numeric(as.character(df$Distance.Bin))
    return(df)}}



