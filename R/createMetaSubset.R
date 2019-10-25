#' Create Subset
#'
#' This function creates subsets from metadata tables by detecting unique
#' values and using these values to split up the dataset.
#'
#' To enable the combination with zscores and/or distance tables, the metadata
#' tables have to feature a column featuring the respective texts filename.
#' @param metadata metadata dataframe
#' @param subset_column column featuring categories used for the subsetting
#' @param path_results path to directory where results should be saved
#' @keywords subsets
#' @export
#' @examples
#' createSubsets()


createMetaSubsets <- function(metadata, subset_column, path_results) {
  # Preliminary checks
  if (!is.data.frame(metadata)) {
    stop("Your metadata table is not a data frame!")
  }
  if (!is.character(subset_column)) {
    stop("Your subset_column is not a character object!")
  }
  if (!dir.exists(path_results)) {
    stop("Your results directory does not exist!")
  }
  # Get the unique values from the metadata column
  vars_df <- unique(metadata[, subset_column])
  vars_df <- vars_df[!is.na(vars_df)]

  # Create subset directories
  subsets_dir <- paste0(path_results, "\\subsets")
  for (i in 1:length(vars_df)) {
    if (!dir.exists(subsets_dir)) {
      dir.create(subsets_dir)
    }
    sub <- paste0(subsets_dir, "\\subset_metadata_", subset_column, "_", i, ".csv")
    # Subset does only work interactively, when you use variables, you need
    # to subset with square brackets (see https://stackoverflow.com/questions/17075529/subset-based-on-variable-column-name)
    subset_x <- metadata[metadata[[subset_column]] == vars_df[i], ]
    write.table(subset_x, sub, sep = ";", quote = FALSE, row.names = TRUE,
                col.names = NA)
  }
}



