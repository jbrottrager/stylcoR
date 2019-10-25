#' Test the Significance
#'
#' This function tests the significance of correlations between values from
#' two subsets.
#' @param path_dist_1 path to distance table comprising texts of category A
#' @param path_dist_1 path to distance table comprising texts of category B
#' @param stat_value statistical value that will be used for the comparison,
#' possible values: mean, median, standard_deviation
#' @keywords significance
#' @export
#' @examples
#' testSignificance()

testSignificance <- function(path_dist_1, path_dist_2, stat_value) {
  # Preliminary checks
  if (!file.exists(path_dist_1)) {
    stop("Your distance table 1 does not exist. Please check the path you've
           entered!")
  }
  if (!file.exists(path_dist_2)) {
    stop("Your distance table 2 does not exist. Please check the path you've
           entered!")
  }
  # Read in both distance matrices
  dist1 <- as.matrix(read.table(path_dist_1,
                                header = TRUE,
                                row.names = 1, sep = " ",
                                check.names = FALSE))

  dist2 <- as.matrix(read.table(path_dist_2,
                                header = TRUE,
                                row.names = 1, sep = " ",
                                check.names = FALSE))

  # Choose statistical value and calculate it for every column
  if (stat_value == "mean") {
    col1 <- colMeans(dist1)
    col2 <- colMeans(dist2)
  } else if (stat_value == "median") {
    col1 <- apply(dist1, 2, median)
    col2 <- apply(dist1, 2, median)
  } else if (stat_value == "standard_deviation"){
    col1 <- apply(dist1, 2, sd)
    col2 <- apply(dist1, 2, sd)
  } else {
    stop("Choose either mean, median, or standard_deviation")
  }

  shapiro1 <- shapiro.test(col1)
  shapiro2 <- shapiro.test(col2)
  if (shapiro1$p.value <= 0.05 && shapiro2$p.value <= 0.05) {
    message("Normal Distribution!")
  }

  # Create dataframes featuring the statistical values and the category (1/2)
  df1 <- data.frame(value = col1, category = rep(1, length(col1)))
  df2 <- data.frame(value = col2, category = rep(2, length(col2)))
  # Combine the two dataframes
  df <- rbind(df1, df2)

  # Carry out significance test
  # Unpaired Two-Sample T-Test
  significance <- t.test(df$value, df$category, data = df, paired = TRUE)

  # Create boxplot and save as pdf
  path <- gsub("(.+)(\\\\|/)(.+)_\\d.csv", "\\1\\2\\3.pdf", path_dist_1,
               perl = TRUE)

  plot <- ggpubr::ggboxplot(df, x = "category", y = "value",
                            color = "category",
                            palette = c("#00AFBB", "#E7B800"),
                            ylim = c(0, 2.0))

  # specific syntax for creating a plot in a function:
  # https://github.com/kassambara/survminer/issues/26
  pdf(path, width = 7, height = 7, onefile = TRUE)
  print(plot)
  dev.off()

  subset1 <- gsub("(.+)(\\\\|/)(.+)metadata_(.+_\\d).csv", "\\4",
                  path_dist_1, perl = TRUE)
  subset2 <- gsub("(.+)(\\\\|/)(.+)metadata_(.+_\\d).csv", "\\4",
                  path_dist_2, perl = TRUE)
  setting <- gsub("(.+)(.gram.+?)(\\\\|/)subsetted(.+)", "\\2",
                  path_dist_2, perl = TRUE)
  corpus <- gsub("(.+?)(\\\\|/)(results)(\\\\|/)(.+?)(\\\\|/)(.+)", "\\5",
                 path_dist_2, perl = TRUE)

  message("Comparison of subset ", subset1, " and subset ", subset2,
          " in the ", corpus, " (", setting, "): ")

  # Is the difference significant?
  if (significance$p.value >= 0.05) {
    message("The results are not significant, p-value of ",
            significance$p.value, "\n")
    sig <- "not significant"
  } else {
    message("The results are significant, p-value of ", significance$p.value,
            "\n")
    sig <- "significant"
  }



  # Save subset names, settings, significant/not signiicant, and p-value
  result <- data.frame(subset1, subset2, corpus, setting, sig,
                       significance$p.value)

  return(result)
}


