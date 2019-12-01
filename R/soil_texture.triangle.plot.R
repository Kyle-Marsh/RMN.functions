#' @title Make a soil texture triangle
#'
#' @description From output of prepare.soil.triangle() creates a soil texture plot and compares a ranch to other ranches in the dataset
#'
#' @param data dataframe, output of prepare.soil.triangle()
#' @param transect Character string of selected ranch code
#' @param year The year or years for which to make the plot
#' @param background = TRUE whether to display "background" data on plot not from selected ranch
#' @param labels = TRUE determines whether points from the chosen ranch are labeled on the plot
#' @param legend Boolean whether to display a legend
#' @param colors colors to plot points on the triangle
#' @param legendtitle Title for the legend
#' @param ... arguments supplied to TT.plot
#'
#' @return Soil classification triangle with chosen ranch points emboldened
#'
#' @examples texture.triangle.plot(soil, "ranchname")
#'
#'
#' @export texture.triangle.plot
#'
#' @return soil texture triangle
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

texture.triangle.plot = function(data, transect, year,
                                 labels = TRUE,
                                 background = TRUE,
                                 pch = 16,
                                 main = paste("Soil Texture - ",transect),
                                 cex.axis = 0.5,
                                 cex.lab = 0.75,
                                 cex.main = 0.75,
                                 text.tol = 0.2,
                                 tri.sum.tst = FALSE,
                                 cex = 0.75,
                                 frame.bg.col = "white",
                                 grid.show = FALSE,
                                 legend = TRUE,
                                 colors = c("black", "gray"),
                                 legendtitle = "Ranch"){

  library(soiltexture)
  data = subset(data, YEAR %in% year)
  if(!background){data = subset(data, subset = Transect %in% transect)}
  data = prepare.soil.triangle(data)
  data$color = NULL
  data$color[data$Transect %in% transect] = colors[1]
  data$color[!(data$Transect %in% transect)] = colors[2]
  data = arrange(data, desc(color))


  data1 = subset(data, data$Transect %in% transect)
  data2 = subset(data, !(data$Transect %in% transect))

  if(isTRUE(labels)){
    l = data1$Point
  } else{l = NA}


  ##texture plot##

  test = TT.plot(
    class.sys = "USDA.TT",    ## with the UDSA texture classes
    tri.data = data,
    pch = 16,
    main = main,
    cex.axis = cex.axis,
    cex.lab = cex.axis,
    cex.main = cex.main,
    text.tol = text.tol,
    tri.sum.tst = tri.sum.tst,
    col = data$color,
    cex = cex,
    frame.bg.col = frame.bg.col,
    grid.show = grid.show
  )


  TT.text(
    tri.data = data1,
    geo = test,
    tri.sum.tst=tri.sum.tst,
    labels = l,
    pos=c(1,3,2),
    offset=0.2,
    font = 0.1,
    cex = cex,
    col= "black")

  if(legend){legend(x = 90, y = 90,
                    title = legendtitle,
                    legend = c(transect, if(background){"Others"}),
                    col = c(rep(colors[1], times = length(transect)), if(background){colors[2]}),
                    pch = pch)}
  }
