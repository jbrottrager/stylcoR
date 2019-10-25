#' Extract Filenames
#'
#' This function extracts all filenames of a corpus for further use.
#' @param path_to_corpus path to the corpus directory
#' @keywords filenames
#' @export
#' @examples
#' extractFilenames()

extractFilenames <- function(path_to_corpus) {
  list_of_files <- dir(path_to_corpus, full.names = TRUE)
  if (length(list_of_files) < 0) {
    stop("There are no texts in your corpus directory. Check your path!")
  }
  filenames <- rep(NA, length(list_of_files))
  for (i in 1:length(list_of_files)) {
    file_name <- gsub("(.+/)([A-Z].+)(.txt)", "\\2", list_of_files[i],
                      perl = TRUE)
    filenames[i] <- file_name
  }
  return(filenames)
}

