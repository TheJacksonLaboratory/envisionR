#' @export
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
