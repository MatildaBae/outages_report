# Identify non-outlier values using the IQR rule

Returns a logical vector indicating which values fall within the
interquartile range (IQR) bounds.

## Usage

``` r
remove_outliers(x, na.rm = TRUE, coef = 1.5)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  Logical; whether to remove NA values when computing quantiles.

- coef:

  Numeric; multiplier for the IQR (default is 1.5).

## Value

A logical vector the same length as `x`, where TRUE indicates a value is
not an outlier.
