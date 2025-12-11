
# SAIDI: System Average Interruption Duration Index

# sum of r_i * N_i / N_T
# r_i is the duration of each interruption
# N_i is the number of customers affected by each interruption
# N_T is the total number of customers served = 3,600,382

SAIDI_calc <- function(outages) {
  cust_served <- 3600382
  outages <- outages |> mutate(outage_effect = as.numeric(difftime(RESTDATE, STARTDATE, units = "min")) * TOTALCUSTAFFECTED)

  sum(outages$outage_effect)/cust_served

}
