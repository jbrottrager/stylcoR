#' Calculate Cosine Delta
#'
#' This function creates a distance table from zscores, using Cosine Delta.
#' @param input_matrix zscores in a matrix; col = features, row = individuals
#' @param already_zscores Is the data already scaled & centered? Default is TRUE
#' @keywords distance table
#' @export
#' @examples
#' calculateCosineDelta()

calculateCosineDelta <- function(input_matrix, already_zscored = TRUE){
  # Preliminary checks
  if (is.matrix(input_matrix) == FALSE) {
    stop("The distance measure can't be applied: input data has wrong format!")
  }
  if (!is.logical(already_zscored)) {
    stop("The variable already_zscored has to be a logical!")
  }
  # Check if input_matrix already includes zscores
  if (already_zscored == FALSE) {
    input_matrix <- calculateZscores(input_matrix)
  }
  message("\n--- Calculating a distance table with Cosine Delta ---\n")
  # Caculate distance table
  dist_table <- as.dist(input_matrix %*% t(input_matrix) /
                         (sqrt(rowSums(input_matrix^2) %*%
                                 t(rowSums(input_matrix^2)))))
  dist_table <- 1 - dist_table
  return(dist_table)
}

