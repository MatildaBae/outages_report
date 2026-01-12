# Summarize daily outages

Creates a daily summary table of outage counts, customers affected,
total duration, average restoration time, and percent momentary events.

## Usage

``` r
summarize_daily_outages(df)
```

## Arguments

- df:

  A data frame of outages with required columns (e.g., STARTDATE,
  RESTDATE, OUTAGEFLAG, TOTALCUSTAFFECTED, MOMENTARYEVENTFLAG).

## Value

A tibble with one row per day.
