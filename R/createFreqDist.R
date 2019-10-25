#' Create a Frequency Table
#'
#' This function creates a distance table by combining several functions
#' @param path_to_corpus path to the corpus directory
#' @param remove_whitespace Should white-spaces be removed? Default is TRUE
#' @param lowercase Should tokens be turned into lowercase? Default is TRUE
#' @param remove_punctuation Should punctuation marks be removed? Default is TRUE
#' @param n_gram_size size of ngrams - possible values: 1 & 2; default is 1
#' @keywords distance table
#' @export
#' @examples
#' createFreqTable()

createFreqTable <- function(path_to_corpus,
                            remove_whitespaces = TRUE,
                            lowercase = TRUE,
                            remove_punctuation = TRUE,
                            n_gram_size = 1) {

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
  if (!is.logical(remove_whitespaces)) {
    stop("Choose a logical value for remove_whitespace!")
  }
  if (!is.logical(lowercase)) {
    stop("Choose a logical value for lowercase!")
  }
  if (!is.logical(remove_punctuation)) {
    stop("Choose a logical value for remove_punctuation!")
  }
  # The path to the output directory is created by cutting the name of the
  # corpus plus the preceding slashes (either back- or forward slashed)
  output_dir <- paste0(gsub("(.+)(\\\\|/).+", "\\1", path_to_corpus,
                            perl = TRUE), "\\results")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  corpus_name <- gsub("(.+)(\\\\|/)(.+)", "\\3", path_to_corpus,  perl = TRUE)
  output_dir <- paste0(gsub("(.+)(\\\\|/).+", "\\1", path_to_corpus,
                            perl = TRUE), "\\results")

  corpus_dir <- paste0(output_dir, "\\", corpus_name)
  # A results directory is created in the superdirectory of the corpus if it
  # doesn't already exist

  if (!dir.exists(corpus_dir)) {
    dir.create(corpus_dir)
  }

  # Create a quanteda corpus
  tokenized_corpus <- createCorpus(path_to_corpus,
                                   remove_whitespaces,
                                   lowercase,
                                   remove_punctuation)

  # Extract filenames
  filenames <- extractFilenames(path_to_corpus)
  # Create n-grams
  n_grams <- ngram(tokenized_corpus, n_gram_size)
  # Calculate all relative frequency distributions
  freq_dists <- calculateRelFreqDists(n_grams)
  # Create an sort an overall feature list
  sorted_feature_list <- sortOverallFeatureList(n_grams)
  rm(n_grams)
  # Combine all frequency distribution into one matrix
  freq_dist <- combineFreqDists(freq_dists, sorted_feature_list, filenames)
  rm(sorted_feature_list)

  # Create a file containig the frequency table
  write.table(as.matrix(freq_dist),
              paste0(corpus_dir, "\\", n_gram_size,
                     "gram_frequency_table.csv"),
              col.names = NA) # leading empty column

  return(freq_dist)

}



