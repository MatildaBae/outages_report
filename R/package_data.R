#' Path to example outage CSV
#'
#' Returns the file path to the example outage data included with the package.
#'
#' @return A character string.
#' @export
package_data_path <- function() {
  system.file("extdata", "PackageData.csv", package = "outagesreport")
}
