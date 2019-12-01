#' @title Extrapolate Infiltration Time
#'
#' @description For infiltration tests that were stopped before completely finishing, this funtion uses the remaining depth (cm) and the run time (minutes) of the test to extrapolate the time of a complete infiltration.
#'
#' @param height height of water remaining in infiltration ring
#' @param time time that infiltration was stopped. Default is 45. Time is in decimal units so 47:30 would be 47.5
#'
#'
#' @return Extrapolated time of Infiltration
#'
#' @examples extrapolate_inf(1.5, 50)
#'
#' @export extrapolate_inf
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

extrapolate_inf = function(height, time = 45){
  inf = (2.5 - height)
  num = time * 450
  vol = (pi*57.76)*(2.5-height)
  time = num/vol
  minutes = trunc(time)
  seconds = round((time - minutes) * 60, 2)
  hours = minutes/60
  hrs = trunc(hours)
  min = round((hours - hrs) * 60)
  final_time = paste(as.character(hrs), as.character(min), as.character(seconds), sep = ":")
  return(final_time)}












