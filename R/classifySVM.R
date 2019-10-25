#' Classify Texts with SVM
#'
#' This function implements a SVM classification.
#' @param path_zscores path to zscore matrix
#' @param metadata metadata dataframe
#' @param metadata_col name of column of metadata dataframe which should be
#' used for the classification
#' @param col_file_name name of column that comprises the texts file names
#' @param seed set seed for classification, default is 100
#' @keywords classification, SVM
#' @export
#' @examples
#' classifySVM()

classifySVM <- function(path_zscores, metadata, metadata_col, col_file_name,
                        seed = 100) {
    # Preliminary checks
    if (!file.exists(path_zscores)) {
      stop("Your zscores file does not exist. Please check the path you've
           entered!")
    }
    if (!is.data.frame(metadata)) {
      stop("Your metadata table is not a dataframe. Please convert it!")
    }
    if (!is.character(metadata_col)) {
      stop("Your metadata_col is not a character object!")
    }
    if (!is.character(col_file_name)) {
      stop("Your col_file_name is not a character object!")
    }
    # Read in zscores
    data <- read.csv(path_zscores, header = TRUE,
                     sep = " ", stringsAsFactors = FALSE,
                     check.names = FALSE, row.names = 1)

    # Filter metadata df for the column containing the file name and the chosen
    # metadata column
    meta_filtered <- data.frame(metadata[[col_file_name]], metadata[[metadata_col]],
                            stringsAsFactors = FALSE)
    colnames(meta_filtered) <- list(col_file_name, metadata_col)

    # Merge the zscores and the metadata
    merged <- merge(meta_filtered, data,
                  by.x = col_file_name,
                  by.y = "row.names",
                  all.y = TRUE)

    # Use file names as rownames and drop the column containing the file names
    rownames(merged) <- merged[,1]
    merged[,1] <- NULL

    # Turn characters to factors
    merged[, metadata_col] <- as.factor(merged[, metadata_col])
    # Drop rows contaning NAs
    merged_noNA <- na.omit(merged)

    # Set the seed - produce reproducable results
    set.seed(seed)

    # Divide up train and test data
    trainRowNumbers <- caret::createDataPartition(y = merged_noNA[, metadata_col],
                                                p = 0.8, list = FALSE)
    trainData <- merged_noNA[trainRowNumbers,]
    testData <- merged_noNA[-trainRowNumbers,]

    # Set training parameters like cross validation & k-folds
    ctrl <- caret::trainControl(method = "cv", number = 10,
                       savePred = TRUE, classProb = TRUE)

    # Train
    mod <- caret::train(as.formula(paste(as.character(metadata_col), "~ .")),
                        data = trainData, method = "svmLinear", trControl = ctrl)

    # Predict
    predictions <- caret::predict.train(object = mod, testData, type="raw")

    # Create confusion matrix
    conmat <- confusionMatrix(predictions, as.factor(testData[,1]))

    # Combine table actual class - prediction
    testData$predicted <- predictions
    results_prediction <- testData[,c(metadata_col, "predicted")]

    # Get importance of individual features
    gbmImp <- varImp(mod, scale = FALSE)

    path <- gsub("(.+)(\\\\|/)(.+)", "\\1\\2",
                 path_zscores, perl = TRUE)

    # Save importance of features
    write.table(gbmImp$importance, paste0(path, "importance_features_",
                                          metadata_col, ".csv"),
                sep = " ", col.names = NA)

    # Save prediction results
    write.table(results_prediction, paste0(path, "results_prediction_",
                                           metadata_col, ".csv"),
                sep = " ", col.names = NA)

    # Calculate accuracy, precision, & recall
    confusion_matrix <- conmat$table
    true_pos <- confusion_matrix[1,1]
    true_neg <- confusion_matrix[2,2]
    false_pos <- confusion_matrix[1,2]
    false_neg <- confusion_matrix[2,1]

    recall <- true_pos/(true_pos + false_neg)
    precision <- true_pos/(true_pos + false_pos)
    accuracy <- as.numeric(conmat$overall[1])

    setting <- gsub("(.+)(.gram.+?)(\\\\|/)(.+)", "\\2",
                  path_zscores, perl = TRUE)
    corpus <- gsub("(.+?)(\\\\|/)(results)(\\\\|/)(.+?)(\\\\|/)(.+)", "\\5",
                   path_zscores, perl = TRUE)

    # Combine settings with accuracy, precision, & recall in a df
    df <- data.frame(corpus, setting, metadata_col, accuracy, precision, recall)

    message("Classification for ", metadata_col, " with the settings ",
            setting, " in the ", corpus, " done!")

    return(df)

}


