#' A \code{ggplot2} theme for JAX branded content
#'
#' This function returns a \code{ggplot2} theme with the D-DIN font, a JAX
#'  branded font.
#'
#' @param base_size base font size in pts.
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#' @returns a \code{ggplot2} theme for automated publishable plots for JAX.
#' @keywords Envision
#' @export

theme_jax_pub <- function(base_size = 11,
                          base_family = "D-DIN Expanded",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {

  # Ensuring required libraries are in namespace
  if (!requireNamespace("showtext")) {
    stop("The showtext package is suggested for this function.")
  }
  if (!requireNamespace("sysfonts")) {
    stop("The sysfonts package is suggested for this function.")
  }
  stopifnot(requireNamespace("ggplot2"))

  # Loading the D-DIN font
  load_ddin_font(quietly = TRUE)

  # Making the theme.
  t <- ggplot2::theme_grey(base_size = base_size,
                           base_family = base_family,
                           base_line_size = base_line_size,
                           base_rect_size = base_rect_size) %+replace%
    ggplot2::theme(text = ggplot2::element_text(family = base_family, color = "#000100"), # All text is #000100 to allow editing
                   panel.background = ggplot2::element_rect(fill = "#FEFFFF", color = NA), # Panel background is #FEFFFF to allow editing
                   panel.border = ggplot2::element_rect(fill = NA, color = "#000001"), # Panel border and ticks are #000001 to allow editing
                   panel.grid = ggplot2::element_line(color = "#FFFFFE"), # Panel grid is #FFFFFE to allow editing
                   panel.grid.minor = ggplot2::element_line(linewidth = rel(0.5)),
                   strip.background = ggplot2::element_rect(fill = "grey85", color = "#000100"),
                   axis.text = ggplot2::element_text(family = base_family, size = base_size / 1.375),
                   axis.title = ggplot2::element_text(family = base_family, size = base_size),
                   title = ggplot2::element_text(family = base_family, size = 12 * base_size / 11),
                   complete = TRUE
    )
  return(t)
}
