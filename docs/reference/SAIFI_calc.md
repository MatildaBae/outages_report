# Calculate SAIFI (System Average Interruption Frequency Index)

SAIFI is defined as the total number of customer interruptions divided
by the total number of customers served.

## Usage

``` r
SAIFI_calc(df, customers_served = 3600382)
```

## Arguments

- df:

  A data frame containing outage data with columns OUTAGEFLAG and
  TOTALCUSTAFFECTED.

- customers_served:

  Total number of customers served by the utility. Defaults to
  3,600,382.

## Value

A numeric value representing the SAIFI metric.
