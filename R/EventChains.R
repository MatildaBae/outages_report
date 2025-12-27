#' Identify outage event chains within boroughs
#'
#' Flags outages as part of an "event chain" when the start time occurs within
#' a specified gap (in minutes) from the previous outage start within the same borough.
#'
#' @param df A data frame of outages with columns STARTDATE, XBORO, OUTAGEFLAG.
#' @param min_gap Minimum gap in minutes (inclusive).
#' @param max_gap Maximum gap in minutes (inclusive).
#' @return A tibble with additional columns: start_ts, prev_start_ts, gap_mins, is_chain.
#' @export
identify_event_chains <- function(df, min_gap = 5, max_gap = 10) {
  df |>
    dplyr::mutate(
      start_ts = lubridate::ymd_hms(.data$STARTDATE, tz = "UTC", quiet = TRUE)
    ) |>
    dplyr::filter(
      !is.na(.data$start_ts),
      !is.na(.data$XBORO),
      .data$OUTAGEFLAG == 1
    ) |>
    dplyr::group_by(.data$XBORO) |>
    dplyr::arrange(.data$start_ts, .by_group = TRUE) |>
    dplyr::mutate(
      prev_start_ts = dplyr::lag(.data$start_ts),
      gap_mins = as.numeric(difftime(.data$start_ts, .data$prev_start_ts, units = "mins")),
      is_chain = !is.na(.data$gap_mins) &
        .data$gap_mins >= min_gap &
        .data$gap_mins <= max_gap
    ) |>
    dplyr::ungroup()
}

#' Summarise chain events by borough
#'
#' @param chains_df Output from identify_event_chains().
#' @return A tibble with total events, chain events, and percent chain events by borough.
#' @export
summarise_event_chains <- function(chains_df) {
  chains_df |>
    dplyr::group_by(.data$XBORO) |>
    dplyr::summarise(
      total_events     = dplyr::n(),
      chain_events     = sum(.data$is_chain, na.rm = TRUE),
      pct_chain_events = 100 * .data$chain_events / .data$total_events,
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$chain_events))
}

#' Plot chain event counts by borough
#'
#' @param chain_summary_df Output from summarise_event_chains().
#' @return A ggplot object.
#' @export
plot_chain_counts_by_boro <- function(chain_summary_df) {
  chain_summary_df |>
    dplyr::filter(.data$XBORO != "-NDA-") |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = stats::reorder(.data$XBORO, .data$chain_events),
        y = .data$chain_events
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Number of event-chain outages by borough",
      x = "Borough",
      y = "Outages starting within 5-10 minutes of previous outage"
    ) +
    ggplot2::theme_minimal()
}

#' Plot percent of outages in chains by borough
#'
#' @param chain_summary_df Output from summarise_event_chains().
#' @return A ggplot object.
#' @export
plot_chain_percent_by_boro <- function(chain_summary_df) {
  chain_summary_df |>
    dplyr::filter(.data$XBORO != "-NDA-") |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = stats::reorder(.data$XBORO, .data$pct_chain_events),
        y = .data$pct_chain_events
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Percent of outages that are part of a 5-10 minute chain",
      x = "Borough",
      y = "Chain events (%)"
    ) +
    ggplot2::theme_minimal()
}

#' Plot time-of-day distribution of chain outages (polar clock)
#'
#' @param chains_df Output from identify_event_chains().
#' @return A ggplot object.
#' @export
plot_chain_time_of_day <- function(chains_df) {
  hour_labels <- c(
    "12 am", "1 am", "2 am", "3 am", "4 am", "5 am",
    "6 am", "7 am", "8 am", "9 am", "10 am", "11 am",
    "12 pm", "1 pm", "2 pm", "3 pm", "4 pm", "5 pm",
    "6 pm", "7 pm", "8 pm", "9 pm", "10 pm", "11 pm"
  )

  chains_df |>
    dplyr::filter(.data$XBORO != "-NDA-") |>
    dplyr::mutate(
      start_ts = lubridate::ymd_hms(.data$STARTDATE, tz = "UTC", quiet = TRUE),
      hour = lubridate::hour(.data$start_ts)
    ) |>
    dplyr::filter(.data$is_chain) |>
    dplyr::count(.data$hour, name = "n") |>
    ggplot2::ggplot(ggplot2::aes(x = .data$hour, y = .data$n)) +
    ggplot2::geom_col(width = 1) +
    ggplot2::coord_polar() +
    ggplot2::scale_x_continuous(
      breaks = 0:23,
      labels = hour_labels,
      limits = c(0, 24)
    ) +
    ggplot2::labs(
      title = "Time-of-day distribution of chain outages",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y      = ggplot2::element_blank()
    )
}

#' Plot time-of-day distribution of chain outages (histogram)
#'
#' @param chains_df Output from identify_event_chains().
#' @return A ggplot object.
#' @export
plot_chain_time_of_day2 <- function(chains_df) {
  chains_df |>
    dplyr::filter(.data$XBORO != "-NDA-") |>
    dplyr::mutate(
      start_ts = lubridate::ymd_hms(.data$STARTDATE, tz = "UTC", quiet = TRUE),
      hour = lubridate::hour(.data$start_ts)
    ) |>
    dplyr::filter(.data$is_chain) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$hour)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = 0, closed = "left") +
    ggplot2::scale_x_continuous(breaks = 0:23) +
    ggplot2::labs(
      title = "Time-of-day distribution of chain outages",
      x = "Hour of day",
      y = "Number of chain outages"
    ) +
    ggplot2::theme_minimal()
}
