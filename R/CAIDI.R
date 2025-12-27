#' Calculate CAIDI (Customer Average Interruption Duration Index)
#'
#' CAIDI is defined as SAIDI divided by SAIFI.
#'
#' @param outages A data frame containing outage data in the required format.
#' @return A numeric value representing the CAIDI metric.
#' @export
CAIDI_calc <- function(outages) {
  SAIDI_calc(outages) / SAIFI_calc(outages)
}
