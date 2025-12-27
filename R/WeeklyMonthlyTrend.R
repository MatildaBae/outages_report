#' Prepare outage records with derived time fields
#'
#' Adds parsed timestamps, durations, and calendar aggregations (day/week/month).
#'
#' @param df A data frame containing STARTDATE, RESTDATE, OUTAGEFLAG, and (optionally) TOTALCUSTAFFECTED.
#' @return A data frame with derived columns.
#' @export
prepare_outages <- function(df) {
  df |>
    dplyr::mutate(
      start_ts = lubridate::ymd_hms(.data$STARTDATE, tz = "UTC", quiet = TRUE),
      rest_ts  = lubridate::ymd_hms(.data$RESTDATE,  tz = "UTC", quiet = TRUE),
      duration_hours = as.numeric(difftime(.data$rest_ts, .data$start_ts, units = "hours")),
      outage_day   = as.Date(.data$start_ts),
      outage_week  = lubridate::floor_date(.data$outage_day, unit = "week", week_start = 1),
      outage_month = lubridate::floor_date(.data$outage_day, unit = "month")
    ) |>
    dplyr::filter(
      !is.na(.data$outage_day),
      !is.na(.data$duration_hours),
      .data$duration_hours >= 0,
      .data$OUTAGEFLAG == 1
    )
}

#' Summarise weekly outages
#'
#' @param df Output from prepare_outages() (must include outage_week, duration_hours).
#' @return A tibble with weekly totals.
#' @export
summarise_weekly_outages <- function(df) {
  df |>
    dplyr::group_by(.data$outage_week) |>
    dplyr::summarise(
      total_outages = dplyr::n(),
      total_customers = sum(rlang::`%||%`(.data$TOTALCUSTAFFECTED, 0), na.rm = TRUE),
      customer_hours_lost = sum(
        rlang::`%||%`(.data$TOTALCUSTAFFECTED, 0) * .data$duration_hours,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$outage_week)
}

#' Summarise monthly outages
#'
#' @param df Output from prepare_outages() (must include outage_month, duration_hours).
#' @return A tibble with monthly totals.
#' @export
summarise_monthly_outages <- function(df) {
  df |>
    dplyr::group_by(.data$outage_month) |>
    dplyr::summarise(
      total_outages = dplyr::n(),
      total_customers = sum(rlang::`%||%`(.data$TOTALCUSTAFFECTED, 0), na.rm = TRUE),
      customer_hours_lost = sum(
        rlang::`%||%`(.data$TOTALCUSTAFFECTED, 0) * .data$duration_hours,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$outage_month)
}

#' Filter a weekly summary by date range
#'
#' @param df A weekly summary data frame with outage_week.
#' @param start_week Optional start week (Date or coercible).
#' @param end_week Optional end week (Date or coercible).
#' @return Filtered data frame.
#' @export
filter_weekly_range <- function(df, start_week = NULL, end_week = NULL) {
  df <- df |>
    dplyr::mutate(outage_week = as.Date(.data$outage_week))

  if (!is.null(start_week)) start_week <- as.Date(start_week)
  if (!is.null(end_week))   end_week   <- as.Date(end_week)

  if (!is.null(start_week) || !is.null(end_week)) {
    if (is.null(start_week)) start_week <- min(df$outage_week, na.rm = TRUE)
    if (is.null(end_week))   end_week   <- max(df$outage_week, na.rm = TRUE)

    df <- df |>
      dplyr::filter(.data$outage_week >= start_week, .data$outage_week <= end_week)
  }

  df
}

#' Filter a monthly summary by date range
#'
#' @param df A monthly summary data frame with outage_month.
#' @param start_month Optional start month (Date or coercible).
#' @param end_month Optional end month (Date or coercible).
#' @return Filtered data frame.
#' @export
filter_monthly_range <- function(df, start_month = NULL, end_month = NULL) {
  df <- df |>
    dplyr::mutate(outage_month = as.Date(.data$outage_month))

  if (!is.null(start_month)) start_month <- as.Date(start_month)
  if (!is.null(end_month))   end_month   <- as.Date(end_month)

  if (!is.null(start_month) || !is.null(end_month)) {
    if (is.null(start_month)) start_month <- min(df$outage_month, na.rm = TRUE)
    if (is.null(end_month))   end_month   <- max(df$outage_month, na.rm = TRUE)

    df <- df |>
      dplyr::filter(.data$outage_month >= start_month, .data$outage_month <= end_month)
  }

  df
}

#' Plot outages per week
#'
#' @param weekly_df Weekly summary output from summarise_weekly_outages().
#' @param start_week Optional start week.
#' @param end_week Optional end week.
#' @return A ggplot object.
#' @export
plot_outages_per_week <- function(weekly_df, start_week = NULL, end_week = NULL) {
  df <- filter_weekly_range(weekly_df, start_week, end_week)
  ymax <- max(df$total_outages, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$outage_week, y = .data$total_outages)) +
    ggplot2::geom_col() +
    ggplot2::geom_line(ggplot2::aes(group = 1), linewidth = 0.7) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_y_continuous(limits = c(0, ymax * 1.1)) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d") +
    ggplot2::labs(x = "Week", y = "Total outages", title = "Outages per week") +
    ggplot2::theme_minimal()
}

#' Plot outages per month
#'
#' @param monthly_df Monthly summary output from summarise_monthly_outages().
#' @param start_month Optional start month.
#' @param end_month Optional end month.
#' @return A ggplot object.
#' @export
plot_outages_per_month <- function(monthly_df, start_month = NULL, end_month = NULL) {
  df <- filter_monthly_range(monthly_df, start_month, end_month)
  ymax <- max(df$total_outages, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$outage_month, y = .data$total_outages)) +
    ggplot2::geom_col() +
    ggplot2::geom_line(ggplot2::aes(group = 1), linewidth = 0.7) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_y_continuous(limits = c(0, ymax * 1.1)) +
    ggplot2::scale_x_date(date_labels = "%Y-%m") +
    ggplot2::labs(x = "Month", y = "Total outages", title = "Outages per month") +
    ggplot2::theme_minimal()
}

#' Plot customer-hours lost per week
#'
#' @param weekly_df Weekly summary output from summarise_weekly_outages().
#' @param start_week Optional start week.
#' @param end_week Optional end week.
#' @return A ggplot object.
#' @export
plot_customer_hours_weekly <- function(weekly_df, start_week = NULL, end_week = NULL) {
  df <- filter_weekly_range(weekly_df, start_week, end_week)
  ymax <- max(df$customer_hours_lost, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$outage_week, y = .data$customer_hours_lost)) +
    ggplot2::geom_col() +
    ggplot2::geom_line(ggplot2::aes(group = 1), linewidth = 0.7) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_y_continuous(limits = c(0, ymax * 1.1)) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d") +
    ggplot2::labs(
      x = "Week",
      y = "Customer-hours lost",
      title = "Total hours customers were without power per week"
    ) +
    ggplot2::theme_minimal()
}

#' Plot customer-hours lost per month
#'
#' @param monthly_df Monthly summary output from summarise_monthly_outages().
#' @param start_month Optional start month.
#' @param end_month Optional end month.
#' @return A ggplot object.
#' @export
plot_customer_hours_monthly <- function(monthly_df, start_month = NULL, end_month = NULL) {
  df <- filter_monthly_range(monthly_df, start_month, end_month)
  ymax <- max(df$customer_hours_lost, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$outage_month, y = .data$customer_hours_lost)) +
    ggplot2::geom_col() +
    ggplot2::geom_line(ggplot2::aes(group = 1), linewidth = 0.7) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_y_continuous(limits = c(0, ymax * 1.1)) +
    ggplot2::scale_x_date(date_labels = "%Y-%m") +
    ggplot2::labs(
      x = "Month",
      y = "Customer-hours lost",
      title = "Total hours customers were without power per month"
    ) +
    ggplot2::theme_minimal()
}

#' Summarise trouble codes monthly
#'
#' @param df Data frame (must include outage_day and TROUBLECODE).
#' @return A tibble of monthly trouble-code outage counts.
#' @export
summarise_troublecode_monthly <- function(df) {
  df |>
    dplyr::mutate(outage_month = lubridate::floor_date(.data$outage_day, "month")) |>
    dplyr::filter(!is.na(.data$TROUBLECODE), .data$TROUBLECODE != "NULL") |>
    dplyr::group_by(.data$outage_month, .data$TROUBLECODE) |>
    dplyr::summarise(outages = dplyr::n(), .groups = "drop_last") |>
    dplyr::arrange(.data$outage_month)
}

#' Plot monthly outages by top trouble codes
#'
#' @param df Data frame of outages.
#' @param top_n_codes Number of top trouble codes to include.
#' @return A ggplot object.
#' @export
plot_troublecode_top_monthly <- function(df, top_n_codes = 5) {
  tc_monthly <- summarise_troublecode_monthly(df)

  top_codes <- tc_monthly |>
    dplyr::group_by(.data$TROUBLECODE) |>
    dplyr::summarise(total_outages = sum(.data$outages), .groups = "drop") |>
    dplyr::slice_max(.data$total_outages, n = top_n_codes) |>
    dplyr::pull(.data$TROUBLECODE)

  tc_monthly |>
    dplyr::filter(.data$TROUBLECODE %in% top_codes) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$outage_month, y = .data$outages, color = .data$TROUBLECODE)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_x_date(date_labels = "%Y-%m") +
    ggplot2::labs(
      x = "Month",
      y = "Outages",
      color = "Trouble code",
      title = "Monthly outages by top trouble codes"
    ) +
    ggplot2::theme_minimal()
}

#' Add a season label based on outage day
#'
#' @param df Data frame containing outage_day.
#' @return Data frame with season factor.
#' @export
add_season <- function(df) {
  df |>
    dplyr::mutate(
      month_num = lubridate::month(.data$outage_day),
      season = dplyr::case_when(
        .data$month_num %in% 3:5 ~ "Spring",
        .data$month_num %in% 6:8 ~ "Summer",
        .data$month_num %in% 9:11 ~ "Fall",
        .data$month_num %in% c(12, 1, 2) ~ "Winter",
        TRUE ~ NA_character_
      ),
      season = factor(.data$season, levels = c("Spring", "Summer", "Fall", "Winter"))
    )
}

#' Summarise outages by season
#'
#' @param df Data frame of outages (must include outage_day and TOTALCUSTAFFECTED).
#' @return A tibble with seasonal totals.
#' @export
summarise_seasonal_outages <- function(df) {
  df |>
    add_season() |>
    dplyr::group_by(.data$season) |>
    dplyr::summarise(
      total_outages = dplyr::n(),
      total_customers = sum(rlang::`%||%`(.data$TOTALCUSTAFFECTED, 0), na.rm = TRUE),
      .groups = "drop"
    )
}

#' Plot seasonal outage frequency
#'
#' @param df Data frame of outages.
#' @return A ggplot object.
#' @export
plot_seasonal_outage_frequency <- function(df) {
  seas <- summarise_seasonal_outages(df)

  ggplot2::ggplot(seas, ggplot2::aes(x = .data$season, y = .data$total_outages)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Season", y = "Total outages", title = "Seasonal outage frequency") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}
