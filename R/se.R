#' @title Calculate standard error
#'
#' @description Calculate standard error of a numeric vector
#' @param x A numeric vector
#'
#'
#' @return Standard error (numeric)
#'
#' @examples se(x)
#'
#' @export se
#'
#'


se <- function(x) sqrt(var(x, na.rm = TRUE)/length(na.omit(x)))
