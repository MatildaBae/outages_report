#' Prepare daily outage time series (tsibble)
#'
#' Aggregates outage-level data into daily totals and constructs a tsibble.
#'
#' @param df A data frame containing outage data with columns STARTDATE, RESTDATE,
#'   OUTAGEFLAG, and TOTALCUSTAFFECTED.
#' @return A tsibble indexed by outage_day with daily totals and a severity index.
#' @export
prepare_daily_ts <- function(df) {
  df |>
    dplyr::mutate(
      start_ts = lubridate::ymd_hms(.data$STARTDATE, tz = "UTC", quiet = TRUE),
      rest_ts  = lubridate::ymd_hms(.data$RESTDATE,  tz = "UTC", quiet = TRUE),
      outage_day = as.Date(.data$start_ts),
      duration_hours = as.numeric(difftime(.data$rest_ts, .data$start_ts, units = "hours"))
    ) |>
    dplyr::filter(
      !is.na(.data$outage_day),
      !is.na(.data$duration_hours),
      .data$duration_hours >= 0,
      .data$OUTAGEFLAG == 1
    ) |>
    dplyr::group_by(.data$outage_day) |>
    dplyr::summarise(
      total_outages = dplyr::n(),
      total_customers = sum(dplyr::coalesce(.data$TOTALCUSTAFFECTED, 0), na.rm = TRUE),
      customer_hours_lost = sum(
        dplyr::coalesce(.data$TOTALCUSTAFFECTED, 0) * .data$duration_hours,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      severity_index = dplyr::if_else(
        .data$total_outages > 0,
        .data$customer_hours_lost / .data$total_outages,
        NA_real_
      )
    ) |>
    tsibble::as_tsibble(index = outage_day)
}

#' Plot daily outage trend using LOESS smoothing
#'
#' @param daily_tsibble A tsibble returned by prepare_daily_ts().
#' @return A ggplot object.
#' @export
plot_outage_trend_loess <- function(daily_tsibble) {
  ggplot2::ggplot(
    daily_tsibble,
    ggplot2::aes(x = .data$outage_day, y = .data$total_outages)
  ) +
    ggplot2::geom_line(alpha = 0.4, linewidth = 0.6) +
    ggplot2::geom_smooth(method = "loess", span = 0.15, se = FALSE, linewidth = 1) +
    ggplot2::labs(
      title = "Smoothed trend of daily outages",
      x = "Outage day",
      y = "Number of outages"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::scale_x_date(date_labels = "%Y-%m")
}
