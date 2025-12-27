#' Identify non-outlier values using the IQR rule
#'
#' Returns a logical vector indicating which values fall within
#' the interquartile range (IQR) bounds.
#'
#' @param x A numeric vector.
#' @param na.rm Logical; whether to remove NA values when computing quantiles.
#' @param coef Numeric; multiplier for the IQR (default is 1.5).
#' @return A logical vector the same length as `x`, where TRUE indicates
#'   a value is not an outlier.
#' @export
remove_outliers <- function(x, na.rm = TRUE, coef = 1.5) {
  qnt <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- qnt[2] - qnt[1]

  x >= (qnt[1] - coef * iqr) &
    x <= (qnt[2] + coef * iqr)
}
