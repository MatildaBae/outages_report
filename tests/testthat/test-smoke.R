testthat::test_that("core functions run", {
  p <- outagesreport::package_data_path()
  df <- readr::read_csv(p, show_col_types = FALSE)

  daily <- outagesreport::summarize_daily_outages(df)
  testthat::expect_true(is.data.frame(daily))

  testthat::expect_true(is.numeric(outagesreport::SAIDI_calc(df)))
  testthat::expect_true(is.numeric(outagesreport::SAIFI_calc(df)))
})
