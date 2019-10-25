#' Limit Most Frequent Features
#'
#' This function limits the features to the designated cut-off.
#' @param freq_dist frequency distribution matrix
#' @param cut_off cut-off for MFF
#' @keywords most frequent features
#' @export
#' @examples
#' cutMFF()

cutMFF <- function(freq_dist, cut_off) {

  # Preliminary checks
  if (!is.data.frame(freq_dist)) {
    stop("Your freq_dist is not a data frame!")
  }
  if (!is.numeric(cut_off)) {
    stop("Your cut_off is not a numeric!")
  }
  message("\n--- Limiting the feature set to the ", cut_off, " most frequent features ---\n")
  # Transpose the matrix for easier modification
  freq_dist <- t(freq_dist)
  if (nrow(freq_dist) >= cut_off) {
    freq_dist <- freq_dist[1:cut_off,]
  }
  else {
    message("\n--- Only ", nrow(freq_dist), "features survived the culling ---\n")
  }
  # Transpose again to get original layout
  return(t(freq_dist))
}
