#' @title Aggregate Soil Data by Point
#'
#' @description Creates a dataframe where each soil point has one row. Each data column is duplicated for each survey event.
#' @description First argument has to be the output of the function survey.sums()
#'
#' @param newdata
#' @param singlesurvey
#'
#' @return Dataframe with soil data from different years merged by point. Columns are added to display percent change in soil data between surveys
#'
#' @examples pair.data(data, singlesurvey = TRUE)
#' @examples paired_data = pair.data(soildata, singlesurvey = FALSE)
#'
#' @export pair.data
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

pair.data = function(newdata, singlesurvey = TRUE){

  b = newdata

  b$surveynumber = NA
  c = b
  c[1:length(rownames(c))] = NULL

  for(i in levels(b$Point)){

    a = filter(b, b$Point == i)
    ind = as.numeric(a$SURVEY)
    a$SURVEY = droplevels(a$SURVEY)
    a$surveynumber = as.numeric(a$SURVEY)

    c = rbind(c, a)

  }

  survey1 = subset(c, c$surveynumber == 1 ,select=c("Transect", "Point", "Carbon.0.10.cm", "Carbon.10.40.cm", "Bulk.Density", "Infilt1", "Clay.10.40.cm", "Sand.10.40.cm", "Silt.10.40.cm"))
  names(survey1)<-c("Transect", "Point", "Carbon.0.10.survey1", "Carbon.10.40.survey1", "Bulk.Density.survey1", "Infilt1.survey1", "Clay.10.40.cm.survey1", "Sand.10.40.cm.survey1", "Silt.10.40.cm.survey1")

  survey2<-subset(c, c$surveynumber == 2, select=c("Point", "Carbon.0.10.cm", "Carbon.10.40.cm", "Bulk.Density", "Infilt1", "Clay.10.40.cm", "Sand.10.40.cm", "Silt.10.40.cm"))
  names(survey2)<-c("Point", "Carbon.0.10.survey2", "Carbon.10.40.survey2", "Bulk.Density.survey2", "Infilt1.survey2", "Clay.10.40.cm.survey2", "Sand.10.40.cm.survey2", "Silt.10.40.cm.survey2")

  Paired_data<-merge(survey1, survey2, by="Point", all = singlesurvey)

  Paired_data$carbon.0.10.per_delta<-((Paired_data$Carbon.0.10.survey2-Paired_data$Carbon.0.10.survey1)/Paired_data$Carbon.0.10.survey1)*100
  Paired_data$carbon.10.40.per_delta<-((Paired_data$Carbon.10.40.survey2-Paired_data$Carbon.10.40.survey1)/Paired_data$Carbon.10.40.survey1)*100
  Paired_data$Bulk.Density.per_delta<-((Paired_data$Bulk.Density.survey2-Paired_data$Bulk.Density.survey1)/Paired_data$Bulk.Density.survey1)*100
  Paired_data$Infilt.per_delta<-((Paired_data$Infilt1.survey2-Paired_data$Infilt1.survey1)/Paired_data$Infilt1.survey1)*100

  return(Paired_data)

}
