#' Prepare data for missingness summaries
#'
#' Optionally converts placeholder "-NDA-" values in XBORO to NA.
#'
#' @param df A data frame.
#' @param treat_ndas Logical; if TRUE, converts "-NDA-" in XBORO to NA (if XBORO exists).
#' @return A data frame.
#' @export
prep_missing_df <- function(df, treat_ndas = TRUE) {
  df_out <- df

  if (isTRUE(treat_ndas) && "XBORO" %in% names(df_out)) {
    df_out <- df_out |>
      dplyr::mutate(XBORO = dplyr::na_if(.data$XBORO, "-NDA-"))
  }

  df_out
}

#' Summarize missing values by variable
#'
#' @param df A data frame.
#' @param treat_ndas Logical; if TRUE, converts "-NDA-" in XBORO to NA before summarising.
#' @return A tibble with columns variable, na_count, na_percent.
#' @export
missing_summary <- function(df, treat_ndas = TRUE) {
  df_use <- prep_missing_df(df, treat_ndas = treat_ndas)
  n <- nrow(df_use)

  summary_tbl <- df_use |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.x)))) |>
    tidyr::pivot_longer(
      cols      = dplyr::everything(),
      names_to  = "variable",
      values_to = "na_count"
    )

  if (n == 0) {
    summary_tbl |>
      dplyr::mutate(na_percent = NA_real_) |>
      dplyr::arrange(dplyr::desc(.data$na_count))
  } else {
    summary_tbl |>
      dplyr::mutate(na_percent = 100 * .data$na_count / n) |>
      dplyr::arrange(dplyr::desc(.data$na_count))
  }
}

#' Return only variables with non-zero missingness
#'
#' @param df A data frame.
#' @param treat_ndas Logical; if TRUE, converts "-NDA-" in XBORO to NA before summarising.
#' @return A tibble filtered to na_count > 0.
#' @export
missing_nonzero <- function(df, treat_ndas = TRUE) {
  missing_summary(df, treat_ndas = treat_ndas) |>
    dplyr::filter(.data$na_count > 0)
}

#' Plot missing counts by variable
#'
#' @param df A data frame.
#' @param treat_ndas Logical; if TRUE, converts "-NDA-" in XBORO to NA before summarising.
#' @param only_nonzero Logical; if TRUE, plots only variables with na_count > 0.
#' @return A ggplot object.
#' @export
plot_missing_bar <- function(df, treat_ndas = TRUE, only_nonzero = TRUE) {
  tbl <- missing_summary(df, treat_ndas = treat_ndas)

  if (isTRUE(only_nonzero)) {
    tbl <- tbl |>
      dplyr::filter(.data$na_count > 0)
  }

  ggplot2::ggplot(
    tbl,
    ggplot2::aes(
      x = stats::reorder(.data$variable, .data$na_count),
      y = .data$na_count
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Missing values by variable",
      x     = "Variable",
      y     = "Number of missing values"
    ) +
    ggplot2::theme_minimal()
}

#' Plot missingness pattern via UpSet plot
#'
#' Wrapper around naniar::gg_miss_upset().
#'
#' @param df A data frame.
#' @param treat_ndas Logical; if TRUE, converts "-NDA-" in XBORO to NA before plotting.
#' @return A ggplot object.
#' @export
plot_missing_upset <- function(df, treat_ndas = TRUE) {
  df_use <- prep_missing_df(df, treat_ndas = treat_ndas)
  naniar::gg_miss_upset(df_use)
}
