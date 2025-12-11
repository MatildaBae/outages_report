suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(fs)
})

prepare_outages <- function(df) {
  df |>
    mutate(
      start_ts = ymd_hms(STARTDATE, tz = "UTC", quiet = TRUE),
      rest_ts  = ymd_hms(RESTDATE,  tz = "UTC", quiet = TRUE),
      duration_hours = as.numeric(difftime(rest_ts, start_ts, units = "hours")),
      outage_day   = as.Date(start_ts),
      outage_week  = floor_date(outage_day, unit = "week", week_start = 1),
      outage_month = floor_date(outage_day, unit = "month")
    ) |>
    filter(
      !is.na(outage_day),
      !is.na(duration_hours),
      duration_hours >= 0,
      OUTAGEFLAG == 1
    )
}

# Weekly & monthly summaries
summarise_weekly_outages <- function(df) {
  df |>
    group_by(outage_week) |>
    summarise(
      total_outages      = n(),
      total_customers    = sum(TOTALCUSTAFFECTED %||% 0, na.rm = TRUE),
      customer_hours_lost = sum((TOTALCUSTAFFECTED %||% 0) * duration_hours,
                                na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(outage_week)
}

summarise_monthly_outages <- function(df) {
  df |>
    group_by(outage_month) |>
    summarise(
      total_outages      = n(),
      total_customers    = sum(TOTALCUSTAFFECTED %||% 0, na.rm = TRUE),
      customer_hours_lost = sum((TOTALCUSTAFFECTED %||% 0) * duration_hours,
                                na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(outage_month)
}

# Plot helpers
filter_weekly_range <- function(df, start_week = NULL, end_week = NULL) {
  df <- df |> mutate(outage_week = as.Date(outage_week))

  if (!is.null(start_week) || !is.null(end_week)) {
    if (is.null(start_week)) start_week <- min(df$outage_week, na.rm = TRUE)
    if (is.null(end_week))   end_week   <- max(df$outage_week, na.rm = TRUE)

    df <- df |>
      filter(outage_week >= start_week,
             outage_week <= end_week)}
  df
}

filter_monthly_range <- function(df, start_month = NULL, end_month = NULL) {
  df <- df |> mutate(outage_month = as.Date(outage_month))

  if (!is.null(start_month) || !is.null(end_month)) {
    if (is.null(start_month)) start_month <- min(df$outage_month, na.rm = TRUE)
    if (is.null(end_month))   end_month   <- max(df$outage_month, na.rm = TRUE)

    df <- df |>
      filter(outage_month >= start_month,
             outage_month <= end_month)
  }
  df
}

# Outages per week
plot_outages_per_week <- function(weekly_df,
                                  start_week = NULL,
                                  end_week   = NULL) {
  df <- filter_weekly_range(weekly_df, start_week, end_week)
  ymax <- max(df$total_outages, na.rm = TRUE)

  ggplot(df, aes(x = outage_week, y = total_outages)) +
    geom_col(fill = "lightblue") +
    geom_line(aes(group = 1), linewidth = 0.7) +
    geom_point(size = 1.8) +
    scale_y_continuous(limits = c(0, ymax * 1.1)) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    labs(
      x = "Week",
      y = "Total outages",
      title = "Outages per week") +
    theme_minimal()
}

# Outages per month
plot_outages_per_month <- function(monthly_df,
                                   start_month = NULL,
                                   end_month   = NULL) {
  df <- filter_monthly_range(monthly_df, start_month, end_month)
  ymax <- max(df$total_outages, na.rm = TRUE)

  ggplot(df, aes(x = outage_month, y = total_outages)) +
    geom_col(fill = "lightblue") +
    geom_line(aes(group = 1), linewidth = 0.7) +
    geom_point(size = 1.8) +
    scale_y_continuous(limits = c(0, ymax * 1.1)) +
    scale_x_date(date_labels = "%Y-%m") +
    labs(
      x = "Month",
      y = "Total outages",
      title = "Outages per month") +
    theme_minimal()
}

# Customer-hours lost (week)
plot_customer_hours_weekly <- function(weekly_df,
                                       start_week = NULL,
                                       end_week   = NULL) {
  df <- filter_weekly_range(weekly_df, start_week, end_week)
  ymax <- max(df$customer_hours_lost, na.rm = TRUE)

  ggplot(df, aes(x = outage_week, y = customer_hours_lost)) +
    geom_col(fill = "aquamarine3") +
    geom_line(aes(group = 1), linewidth = 0.7) +
    geom_point(size = 1.8) +
    scale_y_continuous(limits = c(0, ymax * 1.1)) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    labs(
      x = "Week",
      y = "Customer-hours lost",
      title = "Total hours customers were without power per week") +
    theme_minimal()
}

# Customer-hours lost (month)
plot_customer_hours_monthly <- function(monthly_df,
                                        start_month = NULL,
                                        end_month   = NULL) {
  df <- filter_monthly_range(monthly_df, start_month, end_month)
  ymax <- max(df$customer_hours_lost, na.rm = TRUE)

  ggplot(df, aes(x = outage_month, y = customer_hours_lost)) +
    geom_col(fill = "aquamarine3") +
    geom_line(aes(group = 1), linewidth = 0.7) +
    geom_point(size = 1.8) +
    scale_y_continuous(limits = c(0, ymax * 1.1)) +
    scale_x_date(date_labels = "%Y-%m") +
    labs(
      x = "Month",
      y = "Customer-hours lost",
      title = "Total hours customers were without power per month") +
    theme_minimal()
}


# Troubleshooting code patterns
summarise_troublecode_monthly <- function(df) {
  df |>
    mutate(outage_month = floor_date(outage_day, "month")) |>
    filter(!is.na(TROUBLECODE), TROUBLECODE != "NULL") |>
    group_by(outage_month, TROUBLECODE) |>
    summarise(
      outages = n(),
      .groups = "drop_last") |>
    arrange(outage_month)
}

plot_troublecode_top_monthly <- function(df, top_n_codes = 5) {
  tc_monthly <- summarise_troublecode_monthly(df)

  top_codes <- tc_monthly |>
    group_by(TROUBLECODE) |>
    summarise(total_outages = sum(outages), .groups = "drop") |>
    slice_max(total_outages, n = top_n_codes) |>
    pull(TROUBLECODE)

  tc_monthly |>
    filter(TROUBLECODE %in% top_codes) |>
    ggplot(aes(x = outage_month, y = outages, color = TROUBLECODE)) +
    geom_line() +
    geom_point() +
    scale_x_date(date_labels = "%Y-%m") +
    labs(
      x = "Month",
      y = "Outages",
      color = "Trouble code",
      title = "Monthly outages by top trouble codes") +
    theme_minimal()
}


# Seasonal outage frequency
add_season <- function(df) {
  df |>
    mutate(
      month_num = month(outage_day),
      season = case_when(
        month_num %in% 3:5          ~ "Spring",
        month_num %in% 6:8          ~ "Summer",
        month_num %in% 9:11          ~ "Fall",
        month_num %in% c(12, 1, 2)  ~ "Winter"),
      season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))
    )
}

summarise_seasonal_outages <- function(df) {
  df |>
    add_season() |>
    group_by(season) |>
    summarise(
      total_outages   = n(),
      total_customers = sum(TOTALCUSTAFFECTED %||% 0, na.rm = TRUE),
      .groups = "drop")
}

plot_seasonal_outage_frequency <- function(df) {
  seas <- summarise_seasonal_outages(df)

  ggplot(seas, aes(x = season, y = total_outages, fill = season)) +
    geom_col() +
    labs(
      x = "Season",
      y = "Total outages",
      title = "Seasonal outage frequency") +
    theme_minimal() +
    theme(legend.position = "none")
}
