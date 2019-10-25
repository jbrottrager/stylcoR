#' Sort an Overall Feature List
#'
#' This function creates and sorts an feature list for the entire corpus.
#' @param n_grams a quanteda tokens object containing all tokens
#' in the corpus turned into n-grams
#' @keywords sorted feature list
#' @export
#' @examples
#' sortOverallFeatureList()

sortOverallFeatureList <- function(n_grams)  {
  # Preliminary checks
  if (!is(n_grams,"tokens")) {
    stop("Your variable <n_grams> has the wrong class, should be tokens!")
  }
  message("\n--- Compiling an overall feature list for the entire corpus ---\n")

  # Create an emptly list for storing all features
  features <- rep(list(NA), length(n_grams))

  for (i in 1:length(n_grams)) {
      features[[i]] <- n_grams[[i]]
  }

  # Unlist the created list
  unlisted <- unlist(features)
  # Sort the vector and store it in a table
  sorted_features <- sort(table(unlisted), decreasing = TRUE)
  return(names(sorted_features))
}





