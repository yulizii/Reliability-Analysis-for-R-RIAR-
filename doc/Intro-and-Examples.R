## ----example1-----------------------------------------------------------------
library(riar)
set.seed(123)
g <- function(x) {
  return(x[1]^2 + x[2]^2 - 3)
}
n_samples <- 500  # Define the number of samples
x <- matrix(rnorm(2*n_samples), ncol = 2)
svm_model <- train_svm(x, g)  # Train the SVM model
results <- predict_svm(svm_model, x, g)  # Predict failure probability and reliability index
print(results)  # Print the results

## -----------------------------------------------------------------------------
# Example limit state function
g_example <- function(sample) {
  return(sample[1] + sample[2] - 2)  # Example limit state function
}

# Example sample set
set.seed(123)
samples <- matrix(rnorm(1000), ncol = 2)  # Generate random samples

# Calculate failure probability
result <- MCS(samples, g_example)
print(result)

## -----------------------------------------------------------------------------
library(Rcpp)

# Limist State Function
g <- function(x) {
  x[1] + x[2] - 5
}

# Mean and Variance
mean <- c(3, 2)
sigma <- c(1, 1)

# Result based on form
result <- form(g, mean, sigma)
print(result)


