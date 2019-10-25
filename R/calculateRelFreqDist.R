#' Calculate Relative Frequence Distributions
#'
#' This function calculates relative frequency distributions for every text
#' in the corpus
#' @param n_grams tokens-object comprising all features for each text
#' @keywords relative frequency distribution
#' @export
#' @examples
#' calculateRelFreqDists()

calculateRelFreqDists <- function(n_grams) {
  # Preliminary checks
  if (!is(n_grams,"tokens")) {
    stop("Your variable <n_grams> has the wrong class, should be tokens!")
  }
  # Create an emptly list for storing all frequency distributions
  freq_dists <- rep(list(NA), length(n_grams))

  message("\n--- Computing relative frequencies for all features in all texts ---\n")
  progress_bar <- txtProgressBar(min = 0, max = length(n_grams), style = 3)
  for (i in 1:length(n_grams)) {
    # Calculate relative frequency distribution
    single_freq_dist <- prop.table(table(n_grams[[i]])) * 100
    # Sort frequency distribution
    sorted_freq_dist <- sort(single_freq_dist, decreasing = TRUE)
    # Store frequency distribution in the list
    freq_dists[[i]] <- sorted_freq_dist
    setTxtProgressBar(progress_bar, i)
    gc()
  }
  return(freq_dists)
}
