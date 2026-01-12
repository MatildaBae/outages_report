# Filter a daily outage summary by date range

Filter a daily outage summary by date range

## Usage

``` r
filter_daily_range(daily_df, start_date = NULL, end_date = NULL)
```

## Arguments

- daily_df:

  A daily summary data frame produced by summarize_daily_outages().

- start_date:

  Optional start date (Date or something coercible via as.Date()).

- end_date:

  Optional end date (Date or something coercible via as.Date()).

## Value

Filtered daily_df.
