#' Summarize daily outages
#'
#' Creates a daily summary table of outage counts, customers affected,
#' total duration, average restoration time, and percent momentary events.
#'
#' @param df A data frame of outages with required columns (e.g., STARTDATE, RESTDATE,
#'   OUTAGEFLAG, TOTALCUSTAFFECTED, MOMENTARYEVENTFLAG).
#' @return A tibble with one row per day.
#' @export
summarize_daily_outages <- function(df) {
  df |>
    dplyr::mutate(
      start_ts = lubridate::ymd_hms(.data$STARTDATE, tz = "UTC", quiet = TRUE),
      rest_ts  = lubridate::ymd_hms(.data$RESTDATE,  tz = "UTC", quiet = TRUE),

      outage_day = as.Date(.data$start_ts),
      duration_mins = as.numeric(difftime(.data$rest_ts, .data$start_ts, units = "mins"))
    ) |>
    dplyr::filter(
      !is.na(.data$outage_day),
      !is.na(.data$duration_mins),
      .data$duration_mins >= 0,
      .data$OUTAGEFLAG == 1
    ) |>
    dplyr::group_by(.data$outage_day) |>
    dplyr::summarise(
      total_outages = dplyr::n(),
      total_customers = sum(rlang::`%||%`(.data$TOTALCUSTAFFECTED, 0), na.rm = TRUE),
      total_duration_mins = sum(.data$duration_mins, na.rm = TRUE),
      avg_restoration_mins = mean(.data$duration_mins, na.rm = TRUE),
      pct_momentary_events = 100 * mean(.data$MOMENTARYEVENTFLAG == 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$outage_day)
}

#' Filter a daily outage summary by date range
#'
#' @param daily_df A daily summary data frame produced by summarize_daily_outages().
#' @param start_date Optional start date (Date or something coercible via as.Date()).
#' @param end_date Optional end date (Date or something coercible via as.Date()).
#' @return Filtered daily_df.
#' @export
filter_daily_range <- function(daily_df, start_date = NULL, end_date = NULL) {
  daily_df <- daily_df |>
    dplyr::mutate(outage_day = as.Date(.data$outage_day))

  if (!is.null(start_date)) start_date <- as.Date(start_date)
  if (!is.null(end_date))   end_date   <- as.Date(end_date)

  if (!is.null(start_date) || !is.null(end_date)) {
    if (is.null(start_date)) start_date <- min(daily_df$outage_day, na.rm = TRUE)
    if (is.null(end_date))   end_date   <- max(daily_df$outage_day, na.rm = TRUE)

    daily_df <- daily_df |>
      dplyr::filter(.data$outage_day >= start_date, .data$outage_day <= end_date)
  }

  daily_df
}

#' Plot total outages per day
#'
#' @param daily_df A daily summary data frame.
#' @param start_date Optional start date.
#' @param end_date Optional end date.
#' @return A ggplot object.
#' @export
plot_daily_outages <- function(daily_df, start_date = NULL, end_date = NULL) {
  df <- filter_daily_range(daily_df, start_date, end_date)
  ymax <- max(df$total_outages, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$outage_day, y = .data$total_outages)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(limits = c(0, ymax * 1.1)) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d") +
    ggplot2::labs(x = "Date", y = "Total outages", title = "Total outages per day") +
    ggplot2::theme_minimal()
}

#' Plot total customers affected per day
#'
#' @param daily_df A daily summary data frame.
#' @param start_date Optional start date.
#' @param end_date Optional end date.
#' @return A ggplot object.
#' @export
plot_daily_customers <- function(daily_df, start_date = NULL, end_date = NULL) {
  df <- filter_daily_range(daily_df, start_date, end_date)
  ymax <- max(df$total_customers, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$outage_day, y = .data$total_customers)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(limits = c(0, ymax * 1.1)) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d") +
    ggplot2::labs(x = "Date", y = "Total customers affected",
                  title = "Total customers affected per day") +
    ggplot2::theme_minimal()
}

#' Plot total outage duration (minutes) per day
#'
#' @param daily_df A daily summary data frame.
#' @param start_date Optional start date.
#' @param end_date Optional end date.
#' @return A ggplot object.
#' @export
plot_daily_total_duration <- function(daily_df, start_date = NULL, end_date = NULL) {
  df <- filter_daily_range(daily_df, start_date, end_date)
  ymax <- max(df$total_duration_mins, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$outage_day, y = .data$total_duration_mins)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(limits = c(0, ymax * 1.1)) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d") +
    ggplot2::labs(x = "Date", y = "Total outage minutes",
                  title = "Total duration of outages per day") +
    ggplot2::theme_minimal()
}

#' Plot average restoration time (minutes) per day
#'
#' @param daily_df A daily summary data frame.
#' @param start_date Optional start date.
#' @param end_date Optional end date.
#' @return A ggplot object.
#' @export
plot_daily_avg_restoration <- function(daily_df, start_date = NULL, end_date = NULL) {
  df <- filter_daily_range(daily_df, start_date, end_date)
  ymax <- max(df$avg_restoration_mins, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$outage_day, y = .data$avg_restoration_mins)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(limits = c(0, ymax * 1.1)) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d") +
    ggplot2::labs(x = "Date", y = "Average restoration time (minutes)",
                  title = "Average time to restoration per day") +
    ggplot2::theme_minimal()
}

#' Plot percent momentary events per day
#'
#' @param daily_df A daily summary data frame.
#' @param start_date Optional start date.
#' @param end_date Optional end date.
#' @return A ggplot object.
#' @export
plot_daily_pct_momentary <- function(daily_df, start_date = NULL, end_date = NULL) {
  df <- filter_daily_range(daily_df, start_date, end_date)
  ymax <- max(df$pct_momentary_events, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$outage_day, y = .data$pct_momentary_events)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(limits = c(0, ymax * 1.1)) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d") +
    ggplot2::labs(x = "Date", y = "% momentary events",
                  title = "Percent of momentary events per day") +
    ggplot2::theme_minimal()
}
