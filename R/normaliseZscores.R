#' Normalise Zscores
#'
#' This function transforms zscores to normalised zscores.
#' @param zscores matrix of zscores
#' @keywords zscore transformation
#' @export
#' @examples
#' normaliseZscores()

normaliseZscores <- function(zscores) {
  # Preliminary checks
  if (!is.matrix(zscores)) {
    stop("Your zscores object is not a matrix!")
  }
  matrix <- t(zscores)
  n_col_mat <- ncol(matrix)
  for(i in 1:n_col_mat){
    matrix[,i] <- matrix[,i] / (norm((matrix[,i]), type = c("2")))
  }
  return(t(matrix))
}
