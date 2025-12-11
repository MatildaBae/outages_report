SAIFI_calc <- function(df) {
  cust_served <- 3600382
  outages <- filter(df, OUTAGEFLAG == 1)

  total_interruptions <- sum(outages$TOTALCUSTAFFECTED, na.rm = TRUE)

  sum(total_interruptions)/cust_served
}
