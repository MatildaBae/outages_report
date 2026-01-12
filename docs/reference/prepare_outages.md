# Prepare outage records with derived time fields

Adds parsed timestamps, durations, and calendar aggregations
(day/week/month).

## Usage

``` r
prepare_outages(df)
```

## Arguments

- df:

  A data frame containing STARTDATE, RESTDATE, OUTAGEFLAG, and
  (optionally) TOTALCUSTAFFECTED.

## Value

A data frame with derived columns.
