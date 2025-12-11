remove_outliers <- function(x, na.rm = TRUE, coef = 1.5) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- qnt[2] - qnt[1]
  x >= (qnt[1] - coef * iqr) & x <= (qnt[2] + coef * iqr)
}
