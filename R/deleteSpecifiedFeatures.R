#' Delete Specified Features
#'
#' This function deletes columns from a matrix if the parameter list includes
#' a column's name.
#' @param input_df frequency distribution matrix
#' @param features_to_be_deleted list of tokens which should be deleted
#' @keywords delete features
#' @export
#' @examples
#' deleteSpecifiedFeatures()

deleteSpecifiedFeatures <- function(input_df, features_to_be_deleted) {

  # Preliminary checks
  if (!is.data.frame(input_df)) {
    stop("The input_df is not a data frame!")
  }
  if (!is.character(features_to_be_deleted)) {
    stop("The features_to_be_deleted is not a character vector!")
  }
  culled_data <- input_df[, !c(colnames(input_df) %in%
                                   features_to_be_deleted)]
  return(culled_data)
}
