#' Create Links and Nodes
#'
#' This creates
#' @param path_distance_matrix path to the distance matrix
#' @param nearest_neighbours Should the nearest neighbour method be used?
#' Default is TRUE
#' @param num_neighbours Number of nearest neighbours which will be used in
#' the analysis. Default is 6
#' @param cut_off Should the percental cut-off method be used? Default is FALSE
#' @param percentage Percentage of links which should be used for the network.
#' Default is 5
#' @keywords links, nodes
#' @export
#' @examples
#' createLinksNodes()

createLinksNodes <- function(path_distance_matrix,
                             nearest_neighbours = TRUE,
                             num_neighbours = 6,
                             cut_off = FALSE,
                             percentage = 5) {

  # Preliminary checks
  if (!file.exists(path_distance_matrix)) {
    stop("Your distance matrix does not exist. Check our path!")
  }
  if (!is.logical(nearest_neighbours)) {
    stop("Your nearest_neighbours is not a logical.")
  }
  if (!is.logical(cut_off)) {
    stop("Your cut_off is not a logical.")
  }
  if (nearest_neighbours == TRUE & cut_off == TRUE) {
    stop("You can only use either the nearest_neighbours or the cut_off
         method. One of the option has to be FALSE")
  }
  if (nearest_neighbours == FALSE & cut_off == FALSE) {
    stop("You have to use either the nearest_neighbours or the cut_off
         method. One of the option has to be TRUE")
  }
  if (nearest_neighbours == TRUE) {
    if (!is.numeric(num_neighbours)) {
      stop("Your num_neighbours is not a numeric.")
    }
  }
  if (cut_off == TRUE) {
    if (!is.numeric(percentage)) {
      stop("Your percentage is not a numeric.")
    }
  }

  # Read in distance matrix
  distance_matrix <- as.matrix(read.table(path_distance_matrix,
                                          header = TRUE,
                                          row.names = 1,
                                          check.names = FALSE,
                                          sep = " "))

  # Invert the matrix (so that the smallest values are the biggest)
  distance_matrix <-  1/distance_matrix
  distance_matrix[is.infinite(distance_matrix)] <- 0

  # Choose method for filtering links
  if (nearest_neighbours == TRUE) {
    distance_matrix <- getNearestNeighbours(dist = distance_matrix, num = num_neighbours)
    param_name <- paste0("nearest_neighbours_", num_neighbours)
  } else if (cut_off == TRUE){
    # Sort all values in the distance matrix
    sorted <- sort(unlist(c(distance_matrix[1:nrow(distance_matrix),
                                1:ncol(distance_matrix)])), decreasing = TRUE)
    percent_fronteer <- length(sorted)/100 * percentage
    # Determine cut-off
    cutoff <- sorted[percent_fronteer]

    # Replace values lower than the cut-off with 0
    distance_matrix[distance_matrix < cutoff] <- 0
    param_name <- paste0("cut_of_percentage_", percentage)
  } else {
    stop("You need to chose either nearest_neighbours or cut_off!")
  }

  # Create a igraph adjacency matrix
  g1 <- igraph::graph.adjacency(distance_matrix, weighted = TRUE,
                                add.rownames = TRUE)

  # Create a networkD3 from the igrah object, consisting of links and nodes
  x <- networkD3::igraph_to_networkD3(g1)

  file_name_links <- gsub("(.+)(\\\\|/)(.+)", "\\1\\2links_",
                          path_distance_matrix,  perl = TRUE)
  file_name_links <- paste0(file_name_links, param_name, ".csv")
  file_name_nodes <- gsub("(.+)(\\\\|/)(.+)", "\\1\\2nodes_",
                          path_distance_matrix, perl = TRUE)
  file_name_nodes <- paste0(file_name_nodes, param_name, ".csv")

  # Save links and nodes

  write.table(x$links, file_name_links, sep = " ", col.names = NA)
  write.table(x$nodes, file_name_nodes, sep = " ", col.names = NA)

  # Save parameters in the networkD3 object
  x$param <- c(path_distance_matrix, nearest_neighbours,
               num_neighbours, cut_off, percentage)

  return(x)
}


