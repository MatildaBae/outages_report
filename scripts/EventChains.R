suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(fs)
})

# Identify event chains (5-10 minute gaps within borough)
identify_event_chains <- function(df,
                                  min_gap = 5,
                                  max_gap = 10) {

  df |>
    mutate(
      start_ts = ymd_hms(STARTDATE, tz = "UTC", quiet = TRUE)) |>
    filter(
      !is.na(start_ts),
      !is.na(XBORO),
      OUTAGEFLAG == 1) |>
    group_by(XBORO) |>
    arrange(start_ts, .by_group = TRUE) |>
    mutate(
      prev_start_ts = lag(start_ts),
      gap_mins = as.numeric(difftime(start_ts, prev_start_ts, units = "mins")),
      is_chain = !is.na(gap_mins) &
        gap_mins >= min_gap &
        gap_mins <= max_gap) |>
    ungroup()
}

# Chain events per borough
summarise_event_chains <- function(chains_df) {
  chains_df |>
    group_by(XBORO) |>
    summarise(
      total_events      = n(),
      chain_events      = sum(is_chain, na.rm = TRUE),
      pct_chain_events  = 100 * chain_events / total_events,
      .groups = "drop") |>
    arrange(desc(chain_events))
}


# Plots
# Chain counts per borough
plot_chain_counts_by_boro <- function(chain_summary_df) {
  chain_summary_df |>
    filter(XBORO != "-NDA-") |>
    ggplot(aes(x = reorder(XBORO, chain_events), y = chain_events)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Number of event-chain outages by borough",
      x = "Borough",
      y = "Outages starting within 5–10 minutes of previous outage") +
    theme_minimal()
}

# Share of events in chains (%)
plot_chain_percent_by_boro <- function(chain_summary_df) {
  chain_summary_df |>
    filter(XBORO != "-NDA-") |>
    ggplot(aes(x = reorder(XBORO, pct_chain_events), y = pct_chain_events)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Percent of outages that are part of a 5–10 minute chain",
      x = "Borough",
      y = "Chain events (%)") +
    theme_minimal()
}

# Time-of-day pattern (When do chains happen?)
plot_chain_time_of_day <- function(chains_df) {
  # Labels for the clock
  hour_labels <- c(
    "12 am", "1 am", "2 am", "3 am", "4 am", "5 am",
    "6 am", "7 am", "8 am", "9 am", "10 am", "11 am",
    "12 pm", "1 pm", "2 pm", "3 pm", "4 pm", "5 pm",
    "6 pm", "7 pm", "8 pm", "9 pm", "10 pm", "11 pm"
  )

  chains_df |>
    filter(XBORO != "-NDA-") |>
    mutate(
      start_ts = lubridate::ymd_hms(STARTDATE, tz = "UTC", quiet = TRUE),
      hour     = lubridate::hour(start_ts)
    ) |>
    filter(is_chain) |>
    count(hour) |>
    ggplot(aes(x = hour, y = n)) +
    geom_col(width = 1, fill = "grey30") +
    coord_polar() +  # start at top (12 o'clock)
    scale_x_continuous(
      breaks = 0:23,
      labels = hour_labels,
      limits = c(0, 24)             # close the circle
    ) +
    labs(
      title = "Time-of-day distribution of chain outages",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.y      = element_blank()
    )
}


plot_chain_time_of_day2 <- function(chains_df) {
  chains_df |>
    filter(XBORO != "-NDA-") |>
    mutate(
      start_ts = ymd_hms(STARTDATE, tz = "UTC", quiet = TRUE),
      hour = hour(start_ts)) |>
    filter(is_chain) |>
    ggplot(aes(x = hour)) +
    geom_histogram(binwidth = 1, boundary = 0, closed = "left") +
    scale_x_continuous(breaks = 0:23) +
    labs(
      title = "Time-of-day distribution of chain outages",
      x = "Hour of day",
      y = "Number of chain outages") +
    theme_minimal()
}
