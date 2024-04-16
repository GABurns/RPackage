#' @title Calculate Means
#' @author Gareth Burns
#' @param data A data.frame.
#' @param description A Function to calculate mean value of each columnn in a
#'   \code{data.frame}
#' @return A numeric vector.

CalculateMeans <- function(data) {
  apply(data, 2, mean, na.rm = TRUE)
}
