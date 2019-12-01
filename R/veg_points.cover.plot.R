#' @title Create a plot of cover for functional groups at each point
#'
#' @description For veg data, uses lpi data to summarize cover of each functional group at each point.
#' @description NOTE woody vegetation was only recorded starting in 2017
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param transect Ranch for which to make the plot
#' @param type Type of cover to calculate - either "absolute" or "relative"
#' @param surveyyear Years for which to make the plot. Default is all available years
#' @param invasives Boolean that specifies whether to count invasive species as a separate functional group
#' @param legend.position string specifying legend position
#' @param pallete String identifying color scheme passed to scale_fill_brewer
#' @param x.angle Angle of x axis table
#'
#' @return A plot of functional group cover at each point in a dataset
#'
#' @examples data = points.cover.plot(lpi, type = "absolute")
#'
#' @export points.cover.plot
#'
#'
#'

points.cover.plot = function(lpi, transect,
                             type = "absolute",
                             surveyyear = levels(as.factor(lpi$year)),
                             invasives = FALSE,
                             legend.position = "top",
                             pallete = "YlGnBu",
                             x.angle = 45){
  library(ggplot2)
  fct = functional.cover.table(lpi = lpi, type = type, transect = transect, invasives = invasives, surveyyear = surveyyear, includemeta = TRUE)


  abs<- fct[,colnames(fct) != "NumIndices"]
  abs<-melt(abs, id= c("pointyear", "Transect", "Point.Id", "year"))
  # Note, this step may give a warning, but it's okay
  names(abs)<-c("pointyear", "Transect", "Point.Id", "year", "Type", "Cover")

  absolute = ggplot(abs, aes(x = year, y = Cover))

  p = absolute + geom_col(aes(fill = Type), position = position_stack(reverse = TRUE)) +
    # coord_flip() +
    theme(legend.position = legend.position) +
    theme(legend.text = element_text(size=8)) +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = c("olivedrab", "steelblue3", "orange3",
                                 "mediumpurple4", "firebrick4", "tan", "mistyrose", "yellowgreen")) +
    theme(axis.text.x = element_text(angle = x.angle, hjust = 1)) +
    facet_wrap(~Point.Id) +
    theme_bw()
    # coord_flip()
  return(p)
}
