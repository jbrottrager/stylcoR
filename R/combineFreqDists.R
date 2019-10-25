#' Combine Frequency Distributions
#'
#' This function combines all frequency distributions of all texts.
#' @param freq_dists list of frequency distributions
#' @param sorted_features vector containing the sorted overall features
#' @param filenames vector containing the file names of all texts in the corpus
#' @keywords frequency distribution
#' @export
#' @examples
#' combineFreqDists()

combineFreqDists <- function(freq_dists, sorted_features, filenames) {
  # Preliminary checks
  if (!is.list(freq_dists)) {
    stop("Your list of frequency distributions is not a list!")
  }
  if (!is.character(sorted_features)) {
    stop("Your sorted_features is not a character vector!")
  }
  if (!is.character(filenames)) {
    stop("Your filenames is not a character vector!")
  }
  message("\n--- Combining all frequency distributions ---\n")
  # Turn feature list to factors to speed up the calculation
  if (length(sorted_features) > 1000000) {
    sorted_features <- sorted_features[1:1000000]
  }

  current_levels <- sorted_features
  features <- factor(sorted_features, levels = current_levels, ordered = TRUE)

  # Create an empty matrix; rows = texts, cols = features
  m <- matrix(nrow = length(freq_dists), ncol = length(features))

  # Extract single frequency distributions (only features from overall feature
  # list) and add them to the predefined matrix
  progress_bar <- txtProgressBar(min = 0, max = length(freq_dists), style = 3)
  for (i in 1:length(freq_dists)) {
    single_freq_dist <- freq_dists[[i]]
    single_freq_dist_with_all_features <- single_freq_dist[features]
    m[i,] <- single_freq_dist_with_all_features
    rm(single_freq_dist)
    rm(single_freq_dist_with_all_features)
    setTxtProgressBar(progress_bar, i)
    gc()
  }

  colnames(m) <- features
  rownames(m) <- filenames

  # Replace NAs with 0
  m[is.na(m)] <- 0

  return(m)
}
