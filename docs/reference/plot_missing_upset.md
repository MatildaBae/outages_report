# Plot missingness pattern via UpSet plot

Wrapper around naniar::gg_miss_upset().

## Usage

``` r
plot_missing_upset(df, treat_ndas = TRUE)
```

## Arguments

- df:

  A data frame.

- treat_ndas:

  Logical; if TRUE, converts "-NDA-" in XBORO to NA before plotting.

## Value

A ggplot object.
