# Identify outage event chains within boroughs

Flags outages as part of an "event chain" when the start time occurs
within a specified gap (in minutes) from the previous outage start
within the same borough.

## Usage

``` r
identify_event_chains(df, min_gap = 5, max_gap = 10)
```

## Arguments

- df:

  A data frame of outages with columns STARTDATE, XBORO, OUTAGEFLAG.

- min_gap:

  Minimum gap in minutes (inclusive).

- max_gap:

  Maximum gap in minutes (inclusive).

## Value

A tibble with additional columns: start_ts, prev_start_ts, gap_mins,
is_chain.
