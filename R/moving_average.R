#' @export
moving_average <- function(x, n = 15, align = "center") {
  stopifnot(
    "align must be left, right, or center" =
      align %in% c("left", "right", "center")
  )
  stopifnot("x must be numeric" = is.numeric(x))
  stopifnot(
    "n must be numeric and length 1" =
      is.numeric(n) & length(n) == 1
  )

  filter <- rep(1 / n, n)

  if (align == "right") {
    result <- stats::filter(x, filter, sides = 1)
  } else if (align == "left") {
    result <- stats::filter(rev(x), filter, sides = 1)
    result <- rev(result)
  } else {
    result <- stats::filter(x, filter, sides = 2)
  }

  return(as.numeric(result))
}
