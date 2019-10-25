#' Create a Distance Table
#'
#' This function creates a distance table by combining several functions
#' @param path_to_corpus path to the corpus directory
#' @param freq_dist frequency distribution matrix
#' @param n_gram_size size of ngrams - possible values: 1 & 2; default is 1
#' @param culling_level gives the opportunity to exclude sparse words: a
#' culling level of 20 means that only words which occur in at least 20% of all
#' texts in the corpus are considered in the analysis, default = 20
#' @param cut_off number of features which will be considered for the analysis,
#' default = 500
#' @param zscores_transformation declares whether the zscores should be normalised or
#' ternarised, default = NULL
#' @param distance_measures determines the distance measure which is used for the
#' calculation of the distance table, default = "burrows-delta"
#' @keywords distance table
#' @export
#' @examples
#' createDistanceTable()

createDistanceTable <- function(path_to_corpus,
                                freq_dist,
                                n_gram_size = 1,
                                culling_level = 20,
                                cut_off = 500,
                                zscores_transformation = NULL,
                                distance_measure = "burrows-delta") {
  # Preliminary checks
  if (!dir.exists(path_to_corpus)) {
    stop("You've entered an invalid path!")
  }
  if (length(dir(path_to_corpus, pattern = ".txt")) < 1) {
    stop("Your corpus directory doesn't contain any text files!")
  }
  if (length(dir(path_to_corpus, pattern = ".txt")) == 1) {
    stop("Your corpus directory contains only one text file!")
  }
  if (!is.matrix(freq_dist)) {
    stop("Your frequency distribution is not a matrix!")
  }
  if (n_gram_size != 1 & n_gram_size != 2) {
    stop("Only unigrams and bigrams are possible!")
  }
  if (!is.numeric(culling_level)) {
    stop("Choose a numeric value for the culling_level!")
  }
  if (culling_level < 0 | culling_level > 100) {
    stop("Choose a numeric value between 0 and 100 for the culling_level!")
  }
  if (!is.numeric(cut_off)) {
    stop("Choose a numeric value for the cut_off!")
  }
  corpus_name <- gsub("(.+)(\\\\|/)(.+)", "\\3", path_to_corpus, perl = TRUE)
  output_dir <- paste0(gsub("(.+)(\\\\|/).+", "\\1", path_to_corpus,
                            perl = TRUE), "\\results")

  corpus_dir <- paste0(output_dir, "\\", corpus_name)
  # A results directory is created in the superdirectory of the corpus if it
  # doesn't already exist

  if (!dir.exists(corpus_dir)) {
    dir.create(corpus_dir)
  }

  # Perform culling
  culled <- performCulling(freq_dist, culling = culling_level)
  culled_size <- ncol(culled)

  if (culled_size <= cut_off) {
    message("Only ", culled_size, " features survived the culling!")
    cut_off <- culled_size
  }

  # MFF cut-off
  cut_dist <- cutMFF(culled, cut_off)

  dist_output_dir <- paste0(corpus_dir, "\\",
                                distance_measure)

  if (!dir.exists(dist_output_dir)) {
    dir.create(dist_output_dir)
  }

  specific_output_dir <- paste0(dist_output_dir, "\\",
                                n_gram_size, "gram_",
                                distance_measure, "_",
                                culling_level, "c_",
                                cut_off, "MFF_",
                                zscores_transformation)

  if (!dir.exists(specific_output_dir)) {
    dir.create(specific_output_dir)
  }

  # Create a file comprising all analysed features with their relative
  # frequency
  write.table(cut_dist, paste0(specific_output_dir, "\\features_rel_freq.csv"),
              col.names = NA)
  zscores <- calculateZscores(cut_dist)

  # Check for normalisation or ternarisation and, when applicable,
  # apply chosen transformation
  if (identical(zscores_transformation, "none")) {
    zscores <- zscores
  }
  else if (identical(zscores_transformation, "normalise")) {
    zscores <- normaliseZscores(zscores)
  }
  else if (identical(zscores_transformation, "ternarise")) {
    zscores <- ternariseZscores(zscores)
  }
  else {
    message("You've chosen an invalid zscores transformation!")
  }

  # Create a file comprising all analysed features with their zscores
  write.table(zscores, paste0(specific_output_dir, "\\features_zscores.csv"),
              col.names = NA)

  # Apply chosen distance measure
  if (distance_measure == "burrows-delta") {
    dist_table <- calculateBurrowsDelta(zscores)
  }
  else if (distance_measure == "cosine-delta") {
    dist_table <- calculateCosineDelta(zscores)
  }
  else {
    stop("You've chosen an invalid distance measure!")
  }


  # Create a file containig the distance table
  write.table(as.matrix(dist_table),
              paste0(specific_output_dir, "\\distance_table.csv"),
              col.names = NA) # leading empty column

  message("####################################################################",
          "\n\n",
          "Creation of distance table for ", corpus_name, " is done", "\n\n",
          "PARAMETERS", "\n\n",
          "ngram: ", n_gram_size, "\n",
          "MFF: ", cut_off, "\n",
          "culling: ", culling_level, "\n",
          "zscore transformation: ", zscores_transformation, "\n",
          "distance measure: ", distance_measure,
          "\n\n",
          "####################################################################")

}



