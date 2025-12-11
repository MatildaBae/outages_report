suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(fs)
  library(tsibble)
  library(fable)
  library(feasts)
  library(slider)
  library(ggdist)
  library(urca)
})

prepare_daily_ts <- function(df) {
  df |>
    mutate(
      start_ts = ymd_hms(STARTDATE, tz = "UTC", quiet = TRUE),
      rest_ts  = ymd_hms(RESTDATE,  tz = "UTC", quiet = TRUE),
      outage_day = as.Date(start_ts),
      duration_hours = as.numeric(difftime(rest_ts, start_ts, units = "hours"))) |>
    filter(
      !is.na(outage_day),
      !is.na(duration_hours),
      duration_hours >= 0,
      OUTAGEFLAG == 1) |>
    group_by(outage_day) |>
    summarise(
      total_outages   = n(),
      total_customers = sum(coalesce(TOTALCUSTAFFECTED, 0), na.rm = TRUE),
      customer_hours_lost = sum(coalesce(TOTALCUSTAFFECTED, 0) * duration_hours,
                                na.rm = TRUE),
      .groups = "drop") |>
    mutate(
      severity_index = if_else(total_outages > 0,
                               customer_hours_lost / total_outages,
                               NA_real_)) |>
    as_tsibble(index = outage_day)
}

# Trend analysis
plot_outage_trend_loess <- function(daily_tsibble) {
  daily_tsibble |>
    ggplot(aes(x = outage_day, y = total_outages)) +
    geom_line(alpha = 0.4, color = "gray40", linewidth = 0.6) +
    geom_smooth(
      method    = "loess",
      span      = 0.15,
      se        = FALSE,
      color     = "#0072B2",
      linewidth = 1
    ) +
    labs(
      title    = "Smoothed Trend of Daily Outages",
      x        = "Outage day",
      y        = "Number of outages"
    ) +
    theme_minimal(base_size = 13) +
    scale_x_date(date_labels = "%Y-%m")
}
