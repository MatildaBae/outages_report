2025-12-11 Jiwon Bae and Hillary Rodriguez

# Electricity Outage Reporting 

<!-- badges: start -->
[![R-CMD-check](https://github.com/MatildaBae/outages_report/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MatildaBae/outages_report/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

We are creating a package that takes in outage data and generates a report consisting 
of different graphs and functions.

We provide functions for common utility metrics such as SAIDI - System Average Interruption
Duration Index, SAIFI - System Average Interruption Frequency Index, and CAIDI - 
Customer Average Interruption Duration Index.

We also provide a variety a visuals so users can understand daily, weekly, monthly, 
and yearly outage trends as well as borough-level outage trends.

Currently, the package takes in a CSV file. One of the key assumptions we make is 
that the data used when calling this package has a specific format, as defined 
in our data file.

Our package was designed to be used by utilities to get a better understanding of
their data in a quick, easy, and convenient way.

---

## Installation

### Install from GitHub (recommended)

```r
# install.packages("devtools")
devtools::install_github("MatildaBae/outages_report")
```

### Install from local source (development)

If you have cloned the repository locally, run the following from the package
root directory (the folder containing `DESCRIPTION`):

```r
devtools::install()
```

---

## Instructions

### 1. Load the package

```r
library(outagesreport)
library(readr)
```

---

### 2. Load outage data

You can either use the **example dataset included with the package** or provide
your own CSV file.

#### Option A: Use example data (recommended for first use)

```r
df <- readr::read_csv(package_data_path())
```

#### Option B: Use your own data

```r
df <- readr::read_csv("path/to/your/outage_data.csv")
```

#### Required columns

At minimum, your data should include:

- `STARTDATE`
- `RESTDATE`
- `OUTAGEFLAG`
- `TOTALCUSTAFFECTED`
- `XBORO` (for borough-level and event chain analyses)

Dates must be parseable by `lubridate::ymd_hms()`.

---

### 3. Compute reliability metrics

```r
SAIDI_calc(df)
SAIFI_calc(df)
CAIDI_calc(df)
```

These functions return numeric values representing standard utility reliability
indices.

---

### 4. Generate daily summaries

```r
daily <- summarize_daily_outages(df)
```

This produces a daily-level summary with outage counts, customer impact,
restoration times, and momentary event statistics.

---

### 5. Create plots

```r
plot_daily_outages(daily)
plot_daily_customers(daily)
plot_daily_total_duration(daily)
plot_daily_avg_restoration(daily)
```

All plotting functions return `ggplot` objects and can be customized further.

---

### 6. Analyze event chains (optional)

Event chains identify outages occurring within short time gaps
(5–10 minutes) within the same borough.

```r
chains <- identify_event_chains(df)
chain_summary <- summarise_event_chains(chains)

plot_chain_counts_by_boro(chain_summary)
plot_chain_time_of_day(chains)
```

---

### 7. Explore weekly, monthly, and seasonal trends (optional)

```r
prepared <- prepare_outages(df)

weekly  <- summarise_weekly_outages(prepared)
monthly <- summarise_monthly_outages(prepared)

plot_outages_per_week(weekly)
plot_outages_per_month(monthly)
```

Seasonal summaries:

```r
plot_seasonal_outage_frequency(prepared)
```

---

## Typical workflow

```r
df <- readr::read_csv(package_data_path())

daily <- summarize_daily_outages(df)

SAIDI_calc(df)
plot_daily_outages(daily)
```

---

## Data assumptions

- Input data follows a standardized outage schema
- Dates are interpreted in UTC
- Example data is included for demonstration and testing purposes
