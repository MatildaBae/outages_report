# Plot missing counts by variable

Plot missing counts by variable

## Usage

``` r
plot_missing_bar(df, treat_ndas = TRUE, only_nonzero = TRUE)
```

## Arguments

- df:

  A data frame.

- treat_ndas:

  Logical; if TRUE, converts "-NDA-" in XBORO to NA before summarising.

- only_nonzero:

  Logical; if TRUE, plots only variables with na_count \> 0.

## Value

A ggplot object.
