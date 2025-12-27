# outagesreport 0.0.0.9000

## Initial development version

- Implemented standard reliability metrics:
  - SAIDI (System Average Interruption Duration Index)
  - SAIFI (System Average Interruption Frequency Index)
  - CAIDI (Customer Average Interruption Duration Index)
- Added utilities for daily, weekly, monthly, and seasonal outage summaries
- Included visualization helpers for outage counts, customer impact, and duration
- Implemented event chain detection for clustered outages
- Added example outage dataset (CSV) accessible via `package_data_path()`
- Added basic automated tests using `testthat`
