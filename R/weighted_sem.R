#' A Function to Compute Weighted Standard Error of the Mean JAX Envision® activity CSVs
#'
#' This function computes standard error with weights.
#' @param x vector of type \code{numeric} for computing weighted standard error
#' @param w weights to apply to \code{x}
#' @param na.rm Should the function remove \code{NA} obervations?
#'  (defaults to \code{FALSE})
#' @returns the weighted standard error of the dataset.
#' @keywords Envision
#' @export
#' @examples
#' # Making an example dataset
#' x <- c(0, 0, 3, -3, 0, 0, 0, -0.75, -0.75, 0.75, 0.75)
#' wts <- c(3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 1)
#'
#' # Computing weighted standard error (should return 0.5)
#' weighted_sem(x, wts)
#'
#' # Computing weighted standard error with NAs added to the dataset
#' # The function should return 0.5
#' x_na <- c(x, NA)
#' wts_na <- c(wts, 1)
#' weighted_sem(x_na, wts_na, na.rm = TRUE)
#'
weighted_sem <- function(x, w, na.rm = FALSE) {
  # checking the data
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(w))
  stopifnot(length(x) == length(w))
  na_rm <- na.rm
  w[which(is.na(x))] <- NA

  # getting the sum of the weights
  sumweights <- sum(w, na.rm = na_rm)

  # computing the variables needed for weighted sem
  xbar <- sum(w * x, na.rm = na_rm) / sumweights
  weighted_var <- sum(w * ((x - xbar)^2) / (sumweights - 1), na.rm = na_rm)
  kish_ess <- kish_ess(w, na.rm = na_rm)

  # returning the weighted standard error
  return(sqrt(weighted_var) / sqrt(kish_ess))
}

kish_ess <- function(w, na.rm = TRUE) {
  if (na.rm == TRUE) {
    w <- w[which(!is.na(w))]
  }
  return(sum(w)^2 / sum(w^2))
}
