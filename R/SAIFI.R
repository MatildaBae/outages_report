#' Calculate SAIFI (System Average Interruption Frequency Index)
#'
#' SAIFI is defined as the total number of customer interruptions divided
#' by the total number of customers served.
#'
#' @param df A data frame containing outage data with columns
#'   OUTAGEFLAG and TOTALCUSTAFFECTED.
#' @param customers_served Total number of customers served by the utility.
#'   Defaults to 3,600,382.
#' @return A numeric value representing the SAIFI metric.
#' @export
SAIFI_calc <- function(df, customers_served = 3600382) {

  df |>
    dplyr::filter(.data$OUTAGEFLAG == 1) |>
    dplyr::summarise(
      total_interruptions =
        sum(.data$TOTALCUSTAFFECTED, na.rm = TRUE)
    ) |>
    dplyr::pull(.data$total_interruptions) / customers_served
}
