suppressPackageStartupMessages({
  library(tidyverse)
  library(naniar)
})

prep_missing_df <- function(df, treat_ndas = TRUE) {
  df_out <- df

  if (treat_ndas && "XBORO" %in% names(df_out)) {
    df_out <- df_out |>
      mutate(XBORO = na_if(XBORO, "-NDA-"))
  }

  df_out
}

# NA count
missing_summary <- function(df, treat_ndas = TRUE) {
  df_use <- prep_missing_df(df, treat_ndas = treat_ndas)
  n <- nrow(df_use)

  summary_tbl <- df_use |>
    summarise(across(everything(), ~ sum(is.na(.x)))) |>
    pivot_longer(
      everything(),
      names_to  = "variable",
      values_to = "na_count"
    )

  if (n == 0) {
    summary_tbl |>
      mutate(
        na_percent = NA_real_
      ) |>
      arrange(desc(na_count))
  } else {
    summary_tbl |>
      mutate(
        na_percent = 100 * na_count / n
      ) |>
      arrange(desc(na_count))
  }
}

missing_nonzero <- function(df, treat_ndas = TRUE) {
  missing_summary(df, treat_ndas = treat_ndas) |>
    filter(na_count > 0)
}

# Plot na counts
plot_missing_bar <- function(df, treat_ndas = TRUE, only_nonzero = TRUE) {
  tbl <- missing_summary(df, treat_ndas = treat_ndas)

  if (only_nonzero) {
    tbl <- tbl |>
      filter(na_count > 0)
  }

  ggplot(tbl, aes(x = reorder(variable, na_count), y = na_count)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Missing values by variable",
      x     = "Variable",
      y     = "Number of missing values"
    ) +
    theme_minimal()
}

# Missingness pattern via upset plot (naniar)
plot_missing_upset <- function(df, treat_ndas = TRUE) {
  df_use <- prep_missing_df(df, treat_ndas = treat_ndas)

  naniar::gg_miss_upset(df_use)
}
