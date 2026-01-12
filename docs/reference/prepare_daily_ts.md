# Prepare daily outage time series (tsibble)

Aggregates outage-level data into daily totals and constructs a tsibble.

## Usage

``` r
prepare_daily_ts(df)
```

## Arguments

- df:

  A data frame containing outage data with columns STARTDATE, RESTDATE,
  OUTAGEFLAG, and TOTALCUSTAFFECTED.

## Value

A tsibble indexed by outage_day with daily totals and a severity index.
