#' Transform a Tokens Object to n-grams
#'
#' This function returns either uni- or bigrams.
#' @param preprocessed_corpus a quanteda tokens object containing all tokens
#' in the corpus
#' @n_gram_size size of ngrams - possible values: 1 & 2, default = 1
#' @keywords ngram
#' @export
#' @examples
#' ngram()

ngram <- function(preprocessed_corpus, n_gram_size = 1)  {
  if (n_gram_size != 1 & n_gram_size != 2) {
    stop("Only unigrams and bigrams are possible!")
  }
  if (class(preprocessed_corpus) != "tokens") {
    stop("The corpus seems to not have been converted into tokens!")
  }
  if (n_gram_size == 1) {
    message("\n--- Creating a unigram feature list ---\n")
    features <- preprocessed_corpus
  }
  else if (n_gram_size == 2) {
    message("\n--- Creating a bigram feature list ---\n")
    features <- quanteda::tokens_ngrams(preprocessed_corpus, n = 2L,
                                        concatenator = " ")
  }
  return(features)
}





