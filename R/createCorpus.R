#' Create a Corpus
#'
#' This function creates a quanteda corpus
#' @param path_to_corpus path to the corpus directory
#' @param remove_whitespaces determines if whitespaces should be removed,
#' default = TRUE
#' @param lowercase determines if all cases should be turned into lowercase,
#' default = TRUE
#' @param remove_punctuation determines if all punctuation characters should be
#' removed, default = TRUE
#' @keywords corpus
#' @export
#' @examples
#' createCorpus()

createCorpus <- function(path_to_corpus,
                         remove_whitespaces = TRUE,
                         lowercase = TRUE,
                         remove_punctuation = TRUE) {
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
    stop("The variable remove_whitespaces has to be a logical!")
  }
  if (!is.logical(lowercase)) {
    stop("The variable lowercase has to be a logical!")
  }
  if (!is.logical(remove_punctuation)) {
    stop("The variable remove_punctuation has to be a logical!")
  }
  message("\n--- Compiling a quanteda-Corpus ---\n")
  # Create a quanteda corpus
  file_regex <- paste0(path_to_corpus, "\\*.txt")
  corpus <- quanteda::corpus(readtext::readtext(file_regex,
                                          encoding = "UTF-8"))
  # Remove whitespaces if selected
  if (remove_whitespaces == TRUE) {
    message("\n--- Removing whitespaces ---\n")

    corpus_tokenised <- quanteda::tokens(corpus,
                                         remove_separators = remove_whitespaces,
                                         remove_punct = remove_punctuation)
    # Remove punctuation if selected
    if (remove_punctuation == TRUE) {
      message("\n--- Removing punctuation ---\n")
    }

  }
  # Turn to lowercase if selected
  if (lowercase == TRUE) {
    message("\n--- Turning into lowercase ---\n")
    corpus_tokenised <- quanteda::tokens_tolower(corpus_tokenised,
                                                 keep_acronyms = FALSE)
  }

  return(corpus_tokenised)
}
