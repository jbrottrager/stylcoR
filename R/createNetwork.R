#' Create a Network
#'
#' This function creates a network visualisation.
#' @param list_of_links_and_nodes object containing networkD3 links and nodes
#' and parameter settings
#' @param metadata metadata dataframe
#' @param col_file_name name of column that comprises the texts file name
#' @param metadata_col name of column of metadata dataframe which should be
#' used for the colouring of the nodes
#' @keywords network visualisation
#' @export
#' @examples
#' createNetwork()

createNetwork <- function(list_of_links_and_nodes,
                          metadata,
                          col_file_name,
                          metadata_col) {

  # Preliminary checks
  if (!is.data.frame(metadata)) {
    stop("Your metadata table is not a data frame!")
  }
  if (!is.character(col_file_name)) {
    stop("Your col_file_name is not a character object!")
  }
  if (!is.character(metadata_col)) {
    stop("Your metadata_col is not a character object!")
  }
  if (!is.list(list_of_links_and_nodes)) {
    stop("Somethings wrong with your list of links and nodes! Please check
         your output for the function createLinksNodes()")
  }
  # Extract the path to the distance matrix
  path_distance_matrix  <- as.character(list_of_links_and_nodes$param[[1]])
  # Extract links and nodes
  links <- list_of_links_and_nodes$links
  nodes <- list_of_links_and_nodes$nodes

  # Merge nodes and metadata
  nodes_attributes <- merge(nodes, metadata,
                            by.x = "name", by.y = col_file_name, all.x = TRUE,
                            all.y = FALSE)
  # Filter added metadata
  nodes_attributes <- nodes_attributes[, c("name", as.character(metadata_col))]

  message("\n--- Creating a network visualisation ---\n")

  # Set colour scheme
  hp_colours <- RColorBrewer::brewer.pal(10, "Paired")
  colours <- paste(hp_colours, collapse = '", "')
  colourJS <- paste('d3.scaleOrdinal(["', colours, '"])')

  # Create Force Network
  y <- networkD3::forceNetwork(Links = links,
                               Nodes = nodes_attributes,
                               NodeID ='name',
                               Group = as.character(metadata_col),
                               Value = "value",
                               opacity = 0.8,
                               linkDistance = networkD3::JS("function(d){return Math.sqrt(d.value)}"),
                               zoom = TRUE,
                               charge = -25,
                               legend = TRUE,
                               opacityNoHover = 1,
                               colourScale = colourJS)


  file_name_network <- gsub("(.+)(\\\\|/)(.+)", "\\1\\2network_",
                            path_distance_matrix, perl = TRUE)

  # Extract parameter for file names
  if (list_of_links_and_nodes$param[[2]] == TRUE) {
    param_method <- "nearest_neighbours"
    param_num <- as.character(list_of_links_and_nodes$param[[3]])
  } else {
    param_method <- "cut_off"
    param_num <- as.character(list_of_links_and_nodes$param[[5]])
  }


  file_name_network <- paste0(file_name_network, metadata_col, "_",
                              param_method, "_", param_num, ".html")

  # Save network as html
  networkD3::saveNetwork(y, file_name_network, selfcontained = TRUE)
  setting <- gsub("(.+)(.gram.+?)(\\\\|/)(.+)", "\\2",
                  file_name_network, perl = TRUE)
  corpus <- gsub("(.+?)(\\\\|/)(results)(\\\\|/)(.+?)(\\\\|/)(.+)", "\\5",
                 file_name_network, perl = TRUE)
  file <- gsub("(.+?)(\\\\|/)(network.+).html", "\\3",
               file_name_network, perl = TRUE)

  message(file, " \n",
          "setting: ", setting, "\n",
          "corpus: ", corpus)

  return(y)
}

