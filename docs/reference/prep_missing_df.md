# Prepare data for missingness summaries

Optionally converts placeholder "-NDA-" values in XBORO to NA.

## Usage

``` r
prep_missing_df(df, treat_ndas = TRUE)
```

## Arguments

- df:

  A data frame.

- treat_ndas:

  Logical; if TRUE, converts "-NDA-" in XBORO to NA (if XBORO exists).

## Value

A data frame.
