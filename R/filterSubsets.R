#' Filter for Subset
#'
#' This function creates subsets from metadata tables by detecting unique
#' values and using these values to split up the dataset.
#'
#' To enable the combination with zscores and/or distance tables, the metadata
#' tables have to feature a column featuring the respective texts filename.
#' @param path_distance_matrix path to the distance matrix
#' @param file_name_col name of column that comprises the texts file names
#' @param path_results path to directory where results should be saved
#' @keywords subsets
#' @export
#' @examples
#' filterSubsets()

filterDistSubsets <- function(path_distance_matrix, file_name_col,
                              path_results) {

  # Preliminary checks
  if (!file.exists(path_distance_matrix)) {
    stop("Your distance table does not exist. Please check the path you've
           entered!")
  }
  if (!is.character(file_name_col)) {
    stop("Your file_name_col is not a character object!")
  }
  if (!dir.exists(path_results)) {
    stop("Your results directory does not exist!")
  }
  # Read distance matrix
  distance_matrix <- as.matrix(read.table(path_distance_matrix,
                                          header = TRUE,
                                          row.names = 1,
                                          check.names = FALSE))

  # Get subsets
  list_of_metasubsets <- list.files(path = paste0(path_results, "\\subsets"),
                              pattern = "subset_",
                              full.names = TRUE, recursive = TRUE)

  subsetted_dir <- gsub("(.+)(\\\\|/)(.+)", "\\1\\2subsetted_dists",
                          path_distance_matrix, perl = TRUE)

  if (!dir.exists(subsetted_dir)) {
    dir.create(subsetted_dir)
  }

  # Create subsetted distance table for each subset
  for (i in 1:length(list_of_metasubsets)) {
    subset_col <- gsub(".+subset_(.+)_\\d.csv", "\\1", list_of_metasubsets[i])
    subset_meta_category <- gsub(".+subset_(.+_\\d).csv", "\\1", list_of_metasubsets[i])
    generated_meta_subset <- read.csv(list_of_metasubsets[i], sep = ";",
                                 stringsAsFactors = FALSE)
    names <- generated_meta_subset[[file_name_col]]
    subsetted_dist_table <- distance_matrix[
      which(rownames(distance_matrix) %in% names),
      which(colnames(distance_matrix) %in% names)]

    filename <- paste0(subsetted_dir, "\\subsetted_dist_table_",
                       subset_meta_category, ".csv")
    write.table(subsetted_dist_table, filename, sep = " ", col.names = NA)
  }
}

