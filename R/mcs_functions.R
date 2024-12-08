# R/mcs_functions.R

#' Monte Carlo Simulation for Calculating Failure Probability
#'
#' This function performs Monte Carlo simulation to calculate the failure probability
#' based on the provided limit state function.
#'
#' @param x A matrix data frame where each row represents a sample set.
#' @param g A function that calculates the limit state value for a given sample.
#' @return A list containing:
#'   - Pf: The failure probability.
#'   - beta: The reliability index.
#' @export
#' @examples
#' # Example limit state function
#' g_example <- function(sample) {
#'   return(sample[1] + sample[2] - 3)  # Example limit state function
#' }
#'
#' # Example sample set
#' set.seed(123)
#' samples <- matrix(rnorm(10000), ncol = 2)  # Generate random samples
#' # Calculate failure probability
#' result <- MCS(samples, g_example)
#' print(result)
MCS <- function(x, g) {
  # Calculate g function values
  g_values <- apply(x, 1, g)  # Use the externally defined g function

  # Calculate failure probability Pf
  failure_count <- sum(g_values <= 0)
  pf <- failure_count / nrow(x)  # Use the number of rows in the sample
  R <- 1 - pf
  # Calculate beta
  beta <- qnorm(R)
  return(list(Pf = pf, beta = beta))  # Ensure 'pf' is returned correctly
}
