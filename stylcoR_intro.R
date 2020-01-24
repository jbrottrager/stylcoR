################################################################################
################################# Instructions #################################
################################################################################

#### Reproduction of my analyses using a frequency table
#### (--> https://github.com/jbrottrager/stylcoR/frequency_tables.zip)
####
#### 
#### How your data should look like:
####
#### -> The frequency table should be in a directory called "mini_corpus"
#### -> Add another directory called "results"
####
#### Structure:
#### --- corpus_analysis
####      --- mini_corpus
####          --- 1gram_frequency_table.txt
####      --- results
####      --- distinctive_features_corpus.csv
####
#### -> Your metadata table should feature texts as rows and metadata categories
####    as column. One column has to feature the filename of each text (without
####    file extension) so that the metadata can be linked to the files in the
####    corpus.
####

# Variables that have to be altered ############################################

#install.packages("devtools")
library(devtools)
install_github("jbrottrager/stylcoR")
library(stylcoR)
#install.packages("data.table")
library(data.table)

results_paths <- c("C:\\XXX")
path_to_corpus <- "C:\\XXX"

metadata <- read.table("C:\\XXX\\distinctive_features_corpus.csv", 
                       header = TRUE,
                       check.names = FALSE, sep = ";",
                       stringsAsFactors = FALSE)



freq <- fread("C:\\XXX\\mini_corpus\\1gram_frequency_table.txt", 
              header = TRUE, check.names = FALSE, data.table = FALSE)
# View(metadata)
# Check whether your metadata was read in correctly, you might need to change 
# the separator 

# What is the column name of your filename column?
file_name <- "file_name"

metadata_cols <- colnames(metadata)
# Drop column containing the file names and non-binary categories!
# In my case, the non-binary category is featured in column 3 (column 1 
# features the filename)
metadata_cols_binary <- metadata_cols[-c(1,3)]
# Drop only the filename 
metadata_cols_all <- metadata_cols[2:length(metadata_cols)]

################################################################################
# Distance Tables
################################################################################

# Parameter Settings ###########################################################

distance_measures <- list("burrows-delta", "cosine-delta")

MFW <- list(100, 500, 1000, 3000)

culling <- list(20, 50, 80)

zscores_tranformation <- list("none", "normalise", "ternarise")


param_combination <- expand.grid(culling,
                                 MFW,
                                 zscores_tranformation,
                                 distance_measures)


param_combination <- param_combination[!(param_combination$Var3=="ternarise" & 
                                           param_combination$Var4=="cosine-delta"),]

# Frequency Tables and Distance Tables #########################################

freq2 <- freq[,-1]
rownames(freq2) <- freq[,1]

freq <- as.matrix(freq2)

for (j in 1:nrow(param_combination)) {
  
  createDistanceTable(path_to_corpus,
                      freq_dist = freq,
                      n_gram_size = 1,
                      culling_level = as.numeric(param_combination[j,1]),
                      cut_off = as.numeric(param_combination[j,2]),
                      zscores_transformation = 
                        as.character(param_combination[j,3]),
                      distance_measure = 
                        as.character(param_combination[j,4]))
  gc()
  
}


################################################################################
# Significance 
################################################################################

# Subsetting ###################################################################

# Metadata Subsets
for (j in 1:length(results_paths)) {
  for (i in 1:length(metadata_cols_binary)) {
    createMetaSubsets(metadata, subset_column = metadata_cols_binary[i], 
                      results_paths[j])
  }
}


# Random Subsets
for (j in 1:length(results_paths)) {
  subsets_dir <- paste0(results_paths[j], "\\subsets")
  sub1 <- paste0(subsets_dir, "\\subset_metadata_random_1.csv")
  sub2 <- paste0(subsets_dir, "\\subset_metadata_random_2.csv")
  
  num <- nrow(metadata)
  set.seed(100)
  sample_ids <- sample(seq(1, num), num/2, replace = FALSE)
  meta1 <- metadata[sample_ids, ]
  meta2 <- metadata[-sample_ids, ]
  
  write.table(meta1, sub1, sep = ";", quote = FALSE, row.names = TRUE,
              col.names = NA)
  write.table(meta2, sub2, sep = ";", quote = FALSE, row.names = TRUE,
              col.names = NA)
}

dists <- list.files(results_paths, pattern = "distance_table",
                    recursive = TRUE, full.names = TRUE)

for(n in 1:length(results_paths)) {
  for (m in 1:length(dists)) {
    filterDistSubsets(path_distance_matrix = dists[m], 
                      path_results = results_paths[n], 
                      file_name_col = file_name)
  }
}



# Significance Test ############################################################

all_dists <- list.files(results_paths, pattern = "dist_table_.+.csv", 
                        recursive = TRUE, full.names = TRUE)

results_significance <- data.frame(group1 = character(), group2 = character(), 
                                   corpus = character(), settings = character(),
                                   significant = character(), p_value = numeric())

for (j in seq(1, length(all_dists), by = 2)) {
  t <- testSignificance(all_dists[j], all_dists[j+1], "mean")
  results_significance <- rbind(results_significance, t)
}

results_significance$transformation <- results_significance$setting
results_significance$transformation <- gsub(".+_", "",
                                            results_significance$transformation)

results_significance$MFF <- results_significance$setting
results_significance$MFF <- as.numeric(gsub(".+_(\\d+)MFF_.+", "\\1",
                                            results_significance$MFF))

results_significance$measure <- results_significance$setting
results_significance$measure <- gsub(".+_(.+delta)_.+", "\\1",
                                     results_significance$measure)

results_significance$culling <- results_significance$setting
results_significance$culling <- gsub(".+_(.+c)_.+", "\\1",
                                     results_significance$culling)

results_significance$ngram <- results_significance$setting
results_significance$ngram <- as.numeric(gsub("(.)gram_.+", "\\1",
                                              results_significance$ngram))

write.table(results_significance, 
            paste0(results_paths, "\\results_significance_tests.csv"),
            col.names = NA)

################################################################################
# Classify
################################################################################

zscores <- list()
for (i in 1:length(results_paths)) {
  results_burrows <- paste0(results_paths[i], "\\burrows-delta")
  z <- list.files(results_burrows, pattern = "zscores",
                  recursive = TRUE, full.names = TRUE)
  zscores <- append(zscores, z)
}

zscores <- unlist(zscores)

results_classification <- data.frame(setting = character(), 
                                     metadata_col = character(),
                                     accuracy = numeric(), 
                                     precision = numeric(),
                                     recall = numeric())

for (j in 1:length(metadata_cols_binary)) {
  for (i in 1:length(zscores)) {
    classification <- classifySVM(path_zscores = zscores[i], metadata, 
                                  col_file_name = file_name, 
                                  metadata_col = 
                                    as.character(metadata_cols_binary[j]))
    results_classification <- rbind(results_classification, classification)
  }
}

results_classification$transformation <- results_classification$setting
results_classification$transformation <- gsub(".+_", "",
                                              results_classification$transformation)

results_classification$MFF <- results_classification$setting
results_classification$MFF <- as.numeric(gsub(".+_(\\d+)MFF_.+", "\\1",
                                              results_classification$MFF))

results_classification$culling <- results_classification$setting
results_classification$culling <- gsub(".+_(.+c)_.+", "\\1",
                                       results_classification$culling)

results_classification$ngram <- results_classification$setting
results_classification$ngram <- as.numeric(gsub("(.)gram_.+", "\\1",
                                                results_classification$ngram))


write.table(results_classification, 
            paste0(results_paths, "\\results_classification.csv"),
            col.names = NA)



################################################################################
# Network
################################################################################


all_dists <- list.files(results_paths, pattern = "distance_table.csv", 
                        recursive = TRUE, full.names = TRUE)

neigbours <- list(3,6)

param_combination_neigbours <- expand.grid(metadata_cols_all, neigbours)

for (i in 1:length(all_dists)) {
  for (j in 1:nrow(param_combination_neigbours)) {
    graph <- createLinksNodes(path_distance_matrix = all_dists[i], 
                              nearest_neighbours = TRUE,
                              cut_off = FALSE,
                              num_neighbours = 
                                as.numeric(param_combination_neigbours[j,2]))
    
    net <- createNetwork(graph,
                         metadata, col_file_name = file_name,
                         metadata_col = 
                           as.character(param_combination_neigbours[j,1]))
  }
}

cut_off <- list(1,5)

param_combination_cutoff <- expand.grid(metadata_cols_all, cut_off)

for (i in 1:length(all_dists)) {
  for (j in 1:nrow(param_combination_cutoff)) {
    graph <- createLinksNodes(path_distance_matrix = all_dists[i], 
                              nearest_neighbours = FALSE,
                              cut_off = TRUE,
                              percentage = 
                                as.numeric(param_combination_cutoff[j,2]))
    
    net <- createNetwork(graph,
                         metadata, col_file_name = file_name,
                         metadata_col = 
                           as.character(param_combination_cutoff[j,1]))
  }
}

################################################################################
################################################################################