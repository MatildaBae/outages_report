suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(dplyr)
  library(ggplot2)
})

# Daily outage summary function
summarize_daily_outages <- function(df) {
  df |>
    mutate(
      start_ts = lubridate::ymd_hms(STARTDATE, tz = "UTC", quiet = TRUE),
      rest_ts  = lubridate::ymd_hms(RESTDATE,  tz = "UTC", quiet = TRUE),

      # Use start time as the "event day" for aggregation
      outage_day = as.Date(start_ts),
      duration_mins = as.numeric(difftime(rest_ts, start_ts, units = "mins"))
    ) |>
    # Remove clearly bad durations
    filter(!is.na(outage_day),
           !is.na(duration_mins),
           duration_mins >= 0,
           OUTAGEFLAG == 1) |>
    group_by(outage_day) |>
    summarise(
      total_outages          = n(),
      total_customers        = sum(TOTALCUSTAFFECTED %||% 0, na.rm = TRUE),
      total_duration_mins    = sum(duration_mins, na.rm = TRUE),
      avg_restoration_mins   = mean(duration_mins, na.rm = TRUE),
      pct_momentary_events   = 100 * mean(MOMENTARYEVENTFLAG == 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(outage_day)
}

# Plotting helpers
filter_daily_range <- function(daily_df,
                               start_date = NULL,
                               end_date   = NULL) {

  daily_df <- daily_df |>
    mutate(outage_day = as.Date(outage_day))

  if (!is.null(start_date) || !is.null(end_date)) {
    if (is.null(start_date)) {
      start_date <- min(daily_df$outage_day, na.rm = TRUE)
    }
    if (is.null(end_date)) {
      end_date <- max(daily_df$outage_day, na.rm = TRUE)
    }

    daily_df <- daily_df |>
      filter(outage_day >= start_date,
             outage_day <= end_date)
  }

  daily_df
}

# Total outages per day
plot_daily_outages <- function(daily_df,
                               start_date = NULL,
                               end_date   = NULL) {

  df <- filter_daily_range(daily_df, start_date, end_date)
  ymax <- max(df$total_outages, na.rm = TRUE)

  ggplot(df, aes(x = outage_day, y = total_outages)) +
    geom_col() +
    scale_y_continuous(limits = c(0, ymax * 1.1)) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    labs(
      x = "Date",
      y = "Total outages",
      title = "Total outages per day"
    ) +
    theme_minimal()
}

# Total customers affected
plot_daily_customers <- function(daily_df,
                                 start_date = NULL,
                                 end_date   = NULL) {

  df <- filter_daily_range(daily_df, start_date, end_date)
  ymax <- max(df$total_customers, na.rm = TRUE)

  ggplot(df, aes(x = outage_day, y = total_customers)) +
    geom_col() +
    scale_y_continuous(limits = c(0, ymax * 1.1)) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    labs(
      x = "Date",
      y = "Total customers affected",
      title = "Total customers affected per day"
    ) +
    theme_minimal()
}

# Total outage duration
plot_daily_total_duration <- function(daily_df,
                                      start_date = NULL,
                                      end_date   = NULL) {

  df <- filter_daily_range(daily_df, start_date, end_date)
  ymax <- max(df$total_duration_mins, na.rm = TRUE)

  ggplot(df, aes(x = outage_day, y = total_duration_mins)) +
    geom_col() +
    scale_y_continuous(limits = c(0, ymax * 1.1)) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    labs(
      x = "Date",
      y = "Total outage minutes",
      title = "Total duration of outages per day"
    ) +
    theme_minimal()
}

# Average restoration time
plot_daily_avg_restoration <- function(daily_df,
                                       start_date = NULL,
                                       end_date   = NULL) {

  df <- filter_daily_range(daily_df, start_date, end_date)
  ymax <- max(df$avg_restoration_mins, na.rm = TRUE)

  ggplot(df, aes(x = outage_day, y = avg_restoration_mins)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = c(0, ymax * 1.1)) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    labs(
      x = "Date",
      y = "Average restoration time (minutes)",
      title = "Average time to restoration per day"
    ) +
    theme_minimal()
}

# Percent momentary events
plot_daily_pct_momentary <- function(daily_df,
                                     start_date = NULL,
                                     end_date   = NULL) {

  df <- filter_daily_range(daily_df, start_date, end_date)
  ymax <- max(df$pct_momentary_events, na.rm = TRUE)

  ggplot(df, aes(x = outage_day, y = pct_momentary_events)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = c(0, ymax * 1.1)) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    labs(
      x = "Date",
      y = "% momentary events",
      title = "Percent of momentary events per day"
    ) +
    theme_minimal()
}
