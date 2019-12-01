#' @title Creates a subsetted wide data frame for Point Count data
#'
#' @description For the output of bird_wide.data(). This function subsets the wide dataframe by ranch, year and focal species.
#'
#' @param df is a dataframe. Only works with newpc2 created by add.zeros() and it's previous steps.
#' @param transect A ranch code or a list of ranch codes ie."TOKA".
#' @param surveyyear A year or multiple years. ie. c(2016,2018)
#' @param choose_focal_group Three bird focal groups: Grassland, Oak Woodland, and Riparian. Each contain Spp codes that we consider focal species for that habitat type.
#'
#'
#' @return A subsetted wide data frame by focal group
#'
#' @examples bird_wide.focal(newpc2, "TOKA", surveyyear= 2018, choose_focal_group="Grassland")
#'
#' @export bird_wide.focal
#'


bird_wide.focal<-function(df, transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR))), choose_focal_group= c("Grassland", "Oak Woodland", "Riparian")){

  df = subset(df, Transect %in% transect)
  df = subset(df, YEAR %in% surveyyear)
  #subset grassland birds, Ferruginous Hawk, Grasshopper Sparrow , Mountain Plover, Northern Harrier,White-tailed Kite, Western Meadowlark

  grassland<-df[,c(1:5)]
  if("MOPL" %in% colnames(df)){grassland6<-select(df, MOPL)
  grassland<-bind_cols(grassland, grassland6)}else{df<-df}
  if("GRSP" %in% colnames(df)){grassland2<-select(df, GRSP)
  grassland<-bind_cols(grassland, grassland2)}else{df<-df}
  if("SAVS" %in% colnames(df)){grassland3<-select(df, SAVS)
  grassland<-bind_cols(grassland, grassland3)}else{df<-df}
  if("WEME" %in% colnames(df)){grassland8<-select(df, WEME)
  grassland<-bind_cols(grassland, grassland8)}else{df<-df}
  if("LOSH" %in% colnames(df)){grassland4<-select(df, LOSH)
  grassland<-bind_cols(grassland, grassland4)}else{df<-df}
  if("FEHA" %in% colnames(df)){grassland5<-select(df, FEHA)
  grassland<-bind_cols(grassland, grassland5)}else{df<-df}
  if("WTKI" %in% colnames(df)){grassland7<-select(df, WTKI)
  grassland<-bind_cols(grassland, grassland7)}else{df<-df}
  if("NOHA" %in% colnames(df)){grassland9<-select(df, NOHA)
  grassland<-bind_cols(grassland, grassland9)}else{df<-df}
  if("BUOW" %in% colnames(df)){grassland10<-select(df, BUOW)
  grassland<-bind_cols(grassland, grassland10)}else{df<-df}
  if("AMKE" %in% colnames(df)){grassland11<-select(df, AMKE)
  grassland<-bind_cols(grassland, grassland11)}else{df<-df}


  oak<-df[,c(1:5)]
  if("ACWO" %in% colnames(df)){oak1<-select(df, ACWO)
  oak<-bind_cols(oak, oak1)}else{df<-df}
  if("NUWO" %in% colnames(df)){oak2<-select(df, NUWO)
  oak<-bind_cols(oak, oak2)}else{df<-df}
  if("ATFL" %in% colnames(df)){oak3<-select(df, ATFL)
  oak<-bind_cols(oak, oak3)}else{df<-df}
  if("WBNU" %in% colnames(df)){oak4<-select(df, WBNU)
  oak<-bind_cols(oak, oak4)}else{df<-df}
  if("WEBL" %in% colnames(df)){oak5<-select(df, WEBL)
  oak<-bind_cols(oak, oak5)}else{df<-df}
  if("OATI" %in% colnames(df)){oak6<-select(df, OATI)
  oak<-bind_cols(oak, oak6)}else{df<-df}
  if("EUST" %in% colnames(df)){oak7<-select(df, EUST)
  oak<-bind_cols(oak, oak7)}else{df<-df}
  if("YBMA" %in% colnames(df)){oak8<-select(df, YBMA)
  oak<-bind_cols(oak, oak8)}else{df<-df}
  if("HUVI" %in% colnames(df)){oak9<-select(df, HUVI)
  oak<-bind_cols(oak, oak9)}else{df<-df}
  if("BEWR" %in% colnames(df)){oak10<-select(df, BEWR)
  oak<-bind_cols(oak, oak10)}else{df<-df}
  if("BLGR" %in% colnames(df)){oak11<-select(df, BLGR)
  oak<-bind_cols(oak, oak10)}else{df<-df}
  if("CALT" %in% colnames(df)){oak12<-select(df, CALT)
  oak<-bind_cols(oak, oak12)}else{df<-df}
  if("CASJ" %in% colnames(df)){oak13<-select(df, CASJ)
  oak<-bind_cols(oak, oak13)}else{df<-df}
  if("LASP" %in% colnames(df)){oak14<-select(df, LASP)
  oak<-bind_cols(oak, oak14)}else{df<-df}
  if("CAQU" %in% colnames(df)){oak15<-select(df, CAQU)
  oak<-bind_cols(oak, oak15)}else{df<-df}



  riparian<-df[,c(1:5)]
  if("ATFL" %in% colnames(df)){riparian1<-select(df, ATFL)
  riparian<-bind_cols(riparian, riparian1)}else{df<-df}
  if("NOFL" %in% colnames(df)){riparian2<-select(df, NOFL)
  riparian<-bind_cols(riparian, riparian2)}else{df<-df}
  if("NUWO" %in% colnames(df)){riparian3<-select(df, NUWO)
  riparian<-bind_cols(riparian, riparian3)}else{df<-df}
  if("LAZB" %in% colnames(df)){riparian4<-select(df, LAZB)
  riparian<-bind_cols(riparian, riparian4)}else{df<-df}
  if("BEWR" %in% colnames(df)){riparian5<-select(df, BEWR)
  riparian<-bind_cols(riparian, riparian5)}else{df<-df}
  if("SPTO" %in% colnames(df)){riparian6<-select(df, SPTO)
  riparian<-bind_cols(riparian, riparian6)}else{df<-df}
  if("YEWA" %in% colnames(df)){riparian7<-select(df, YEWA)
  riparian<-bind_cols(riparian, riparian7)}else{df<-df}
  if("COYE" %in% colnames(df)){riparian8<-select(df, COYE)
  riparian<-bind_cols(riparian, riparian8)}else{df<-df}
  if("YBCH" %in% colnames(df)){riparian9<-select(df, YBCH)
  riparian<-bind_cols(riparian, riparian9)}else{df<-df}
  if("SOSP" %in% colnames(df)){riparian10<-select(df, SOSP)
  riparian<-bind_cols(riparian, riparian10)}else{df<-df}
  if("BHGR" %in% colnames(df)){riparian11<-select(df, BHGR)
  riparian<-bind_cols(riparian, riparian11)}else{df<-df}
  if("BLGR" %in% colnames(df)){riparian12<-select(df, BLGR)
  riparian<-bind_cols(riparian, riparian12)}else{df<-df}
  if("BUOR" %in% colnames(df)){riparian13<-select(df, BUOR)
  riparian<-bind_cols(riparian, riparian13)}else{df<-df}
  if("WAVI" %in% colnames(df)){riparian14<-select(df, WAVI)
  riparian<-bind_cols(riparian, riparian14)}else{df<-df}




  if(("Grassland" %in% choose_focal_group)){df<-grassland}
  if(("Oak Woodland" %in% choose_focal_group)){df<-oak}
  if(("Riparian" %in% choose_focal_group)){df<-riparian }





  return(df)
}

