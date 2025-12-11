source("SAIDI_code.R")
source("SAIFI_code.R")

CAIDI_calc <- function(outages) {

  SAIDI_calc(outages)/SAIFI_calc(outages)

}
