2025-12-11 Jiwon Bae and Hillary Rodriguez

# Electricity Outage Reporting 

<!-- badges: start -->
[![R-CMD-check](https://github.com/MatildaBae/outages_report/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/MatildaBae/outages_report/actions/workflows/R-CMD-check.yml)
<!-- badges: end -->

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

```r
# install from local source
devtools::install()

