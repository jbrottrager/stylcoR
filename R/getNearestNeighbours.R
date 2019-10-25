#' Get Nearest Neighbours
#'
#' This function leaves only values that belong to the x nearest neighours in
#' a distance matrix, all other values are replaced by 0.
#' @param dist distance matrix
#' @param num number of nearest neighbours
#' @keywords nearest neighbours
#' @export
#' @examples
#' getNearestNeighbours()

getNearestNeighbours <- function(dist, num) {
  # preliminary checks
  if (!is.matrix(dist)) {
    stop("Your distance matrix is not a matrix!")
  }
  if (!is.numeric(num)) {
    stop("Your num is not a numeric!")
  }
  num <- num - 1
  for (i in 1:nrow(dist)) {
    # Rank values in distance table
    ordered <- rank(dist[i,])
    for (j in 1:length(ordered)) {
      # Go through values and check whether rank is in
      if (ordered[j] %in% seq(length(ordered)-num, length(ordered))) {
        change_boolean <- FALSE
      } else {
        change_boolean <- TRUE
      }

      if (change_boolean == TRUE) {
        dist[i,j] <- 0
      }
    }
  }
  return(dist)
}
