#' Perform Culling
#'
#' This function performs culling and returns the culled frequency distribution.
#' @param freq_dist frequency distribution matrix
#' @param culling culling value
#' @keywords culling
#' @export
#' @examples
#' performCulling()

performCulling <- function(freq_dist, culling) {
  # Preliminary checks
  if (!is.matrix(freq_dist)) {
    stop("freq_dist has to be a matrix!")
  }
  if (!is.numeric(culling)) {
    stop("Choose a numeric value for the culling!")
  }
  if (culling < 0 | culling > 100) {
    stop("Choose a numeric value between 0 and 100 for the culling!")
  }
  df <- as.data.frame(freq_dist)

  message("\n--- Performing culling with a culling level of ",
          culling, " % ---\n")

  # In how many texts is a given feature represented?
  # If colSums(df != 0) / nrow(df) == 1, it is represented in all texts
  # If colSums(df != 0) / nrow(df) == 0, it is represented in none
  x <- colSums(df != 0) / nrow(df)
  y <- names(x[x < culling/100])

  # Delete features filtered out in the culling
  culled_freq_dist <- deleteSpecifiedFeatures(df, y)

  return(culled_freq_dist)
}

