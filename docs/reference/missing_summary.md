# Summarize missing values by variable

Summarize missing values by variable

## Usage

``` r
missing_summary(df, treat_ndas = TRUE)
```

## Arguments

- df:

  A data frame.

- treat_ndas:

  Logical; if TRUE, converts "-NDA-" in XBORO to NA before summarising.

## Value

A tibble with columns variable, na_count, na_percent.
