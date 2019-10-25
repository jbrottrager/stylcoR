#' Calculate Burrows Delta
#'
#' This function creates a distance table from zscores, using Burrows' Delta.
#' @param input_matrix zscores in a matrix; col = features, row = individuals
#' @param already_zscores Is the data already scaled & centered? Default is TRUE
#' @keywords distance table
#' @export
#' @examples
#' calculateBurrowsDelta()

calculateBurrowsDelta <- function(input_matrix, already_zscored = TRUE) {
  # Check input format
  if (is.matrix(input_matrix) == FALSE & is.data.frame(input_matrix) == FALSE) {
    stop("The distance measure can't be applied: input data has wrong format!")
  }
  if (!is.logical(already_zscored)) {
    stop("The variable already_zscored has to be a logical!")
  }
  # Check if input_matrix already includes zscores
  if (already_zscored == FALSE) {
    input_matrix <- calculateZscores(input_matrix)
  }
  message("\n--- Calculating a distance table with Burrows Delta ---\n")
  # Caculate distance table
  dist_table <- dist(input_matrix, method = "manhattan") /
                length(input_matrix[1,])
  return(dist_table)
}
