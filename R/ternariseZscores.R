#' Ternarise Zscores
#'
#' This function transforms zscores to ternarised zscores.
#' @param zscores matrix of zscores
#' @keywords zscore transformation
#' @export
#' @examples
#' ternariseZscores()

ternariseZscores <- function(zscores) {
  # Preliminary checks
  if (!is.matrix(zscores)) {
    stop("Your zscores object is not a matrix!")
  }
  n_col_mat = ncol(zscores)
  n_row_mat = nrow(zscores)
  # Go through all values and replace them by 1,-1, or 0
  for(i in 1:n_row_mat){
    for(j in 1:n_col_mat){
      if(zscores[i,j] > 0.43){
        zscores[i,j] <- 1
      }
      else if (zscores[i,j] < -0.43){
        zscores[i,j] <- -1
      }
      else{
        zscores[i,j] <- 0
      }
    }
  }
  return(zscores)
}
