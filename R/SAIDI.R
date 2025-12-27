#' Calculate SAIDI (System Average Interruption Duration Index)
#'
#' SAIDI is defined as the sum of customer interruption durations divided
#' by the total number of customers served.
#'
#' @param outages A data frame containing outage data with columns
#'   STARTDATE, RESTDATE, and TOTALCUSTAFFECTED.
#' @param customers_served Total number of customers served by the utility.
#'   Defaults to 3,600,382.
#' @return A numeric value representing the SAIDI metric (minutes).
#' @export
SAIDI_calc <- function(outages, customers_served = 3600382) {

  outages |>
    dplyr::mutate(
      outage_duration_mins =
        as.numeric(
          difftime(
            lubridate::ymd_hms(.data$RESTDATE, tz = "UTC", quiet = TRUE),
            lubridate::ymd_hms(.data$STARTDATE, tz = "UTC", quiet = TRUE),
            units = "mins"
          )
        ),
      outage_effect =
        .data$outage_duration_mins *
        dplyr::coalesce(.data$TOTALCUSTAFFECTED, 0)
    ) |>
    dplyr::summarise(
      total_effect = sum(.data$outage_effect, na.rm = TRUE)
    ) |>
    dplyr::pull(.data$total_effect) / customers_served
}
