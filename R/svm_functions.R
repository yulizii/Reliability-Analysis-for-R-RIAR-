#/svm_functions.R

#' SVM Model Training and Prediction
#'
#' This function trains a Support Vector Machine (SVM) model on the provided data.
#'
#' @param x The input data (matrix or data frame).
#' @param g The limit state function.
#' @return The SVM modeling result.
#' @export
#' @examples
#' library(riar)
#' set.seed(123)
#' g <- function(x) {
#'   return(x[1]^2 + x[2]^2 - 3)
#' }
#' n_samples <- 500  # Define the number of samples
#' x <- matrix(rnorm(n_samples*2), ncol = 2) # Generate random number
#' svm_model <- train_svm(x, g)  # Train the SVM model
#' print("The svm model has been successfully constructed")  # Print the results
train_svm <- function(x, g) {

  # Check if the e1071 package is installed
  if (!requireNamespace("e1071", quietly = TRUE)) {
    # If not installed, automatically install the e1071 package
    install.packages("e1071")
  }

  # Load the e1071 package
  library(e1071)

  # Use the apply function to compute g values for each row
  g_values <- apply(x, 1, function(row) g(row)) # Calculate g values for each row

  # Generate labels
  y <- ifelse(g_values <= 0, 1, 0)  # Failure event label is 1, non-failure label is 0

  # Train the SVM model
  svm_model <- svm(x, as.factor(y), kernel = "radial")  # Use radial basis kernel

  return(svm_model)
}


#' Predict Failure Probability and Reliability Index Based on the Trained SVM Model
#'
#' This function uses a trained SVM model to predict the failure probability and reliability index
#' based on the provided and limit state function.
#'
#' @param svm_model A trained SVM model (object of class `svm`).
#' @param data_predict A matrix or data frame of samples to be predicted.
#' @param g A function defining the limit state function, which takes a matrix.
#' @return A list containing:
#'   - Pf: Estimated failure probability (proportion of failure events).
#'   - beta: Reliability index (distance to the limit state surface).
#' @export
#' @examples
#' library(riar)
#' set.seed(123)
#' g <- function(x) {
#'   return(x[1]^2 + x[2]^2 - 3)
#' }
#' n_samples <- 500  # Define the number of samples
#' x <- matrix(rnorm(2*n_samples), ncol = 2)
#' svm_model <- train_svm(x, g)  # Train the SVM model
#' results <- predict_svm(svm_model, x, g)  # Predict failure probability and reliability index
#' print(results)  # Print the results
predict_svm <- function(svm_model, data_predict, g) {
  # Use the trained SVM model to make predictions
  predictions <- predict(svm_model, data_predict)

  # Calculate failure probability Pf (proportion of predicted failures)
  Pf <- mean(predictions == "1")
  R <- 1 - Pf

  # Calculate beta
  beta <- qnorm(R)

  # Return failure probability and reliability index
  return(list(Pf = Pf, beta = beta))
}
