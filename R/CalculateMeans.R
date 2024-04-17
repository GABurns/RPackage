#' @title Calculate Means
#' @author Gareth Burns
#' @param data A data.frame.
#' @param description A Function to calculate mean value of each columnn in a
#'   \code{data.frame}
#' @return A numeric vector.

CalculateMeans <- function(data) {
  # Function to return rows with only numeric values in a data frame
  filter_numeric_rows <- function(data) {
    # Check if each row contains only numeric values
    numeric_rows <- sapply(data, function(col) all(sapply(col, is.numeric)))

    if (any(!numeric_rows)) {
      warning(paste("The row", names(data)[!numeric_rows], "contains non-numeric data did not have mean calculated"), call. = FALSE)
    }
    # Return only the rows where all values are numeric
    return(data[, numeric_rows, drop = FALSE])
  }

  data <- filter_numeric_rows(data)

  apply(data, 2, mean, na.rm = TRUE)
}
