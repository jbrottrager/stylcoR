#' Calculate Zscores
#'
#' This function calulates zscores from relative frequency distributions.
#' @param freq_dist matrix comprising the frequency distribution for the
#' surviving features for each text
#' @keywords zscores
#' @export
#' @examples
#' calculateZscores()

calculateZscores <- function(freq_dist) {
  # Preliminary checks
  if (is.matrix(freq_dist) == FALSE) {
    stop("Zscores can't be calculated: input data has wrong format!")
  }
  message("\n--- Calculating the zscores for the relative frequency counts ---\n")
  zscores <- scale(freq_dist)
  zscores <- zscores[,]
  return(zscores)
}
