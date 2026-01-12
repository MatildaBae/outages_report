# Return only variables with non-zero missingness

Return only variables with non-zero missingness

## Usage

``` r
missing_nonzero(df, treat_ndas = TRUE)
```

## Arguments

- df:

  A data frame.

- treat_ndas:

  Logical; if TRUE, converts "-NDA-" in XBORO to NA before summarising.

## Value

A tibble filtered to na_count \> 0.
