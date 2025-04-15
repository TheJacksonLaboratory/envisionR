#' A helper function to load the D-DIN font
#'
#' This function loads an open source version of DIN 1451. This font was
#'  originally prepared for the company Datto and licensed under the OFL v1.1.
#'  The [D-DIN font](https://github.com/amcchord/datto-d-din) is available to
#'  download and redistribute freely. This function does not return any value.
#'
#' @param quietly should the function suppress warnings? (default: \code{FALSE})
#' @returns No return value.
#' @keywords Envision
#' @export
load_ddin_font <- function(quietly = FALSE) {
  # Ensuring required libraries are in namespace
  if (!requireNamespace("showtext")) {
    stop("The showtext package is suggested for this function.")
  }
  if (!requireNamespace("sysfonts")) {
    stop("The sysfonts package is suggested for this function.")
  }

  # Getting paths for fonts
  ddin_regular_path <- system.file("fonts", "D-DIN.ttf",
                                   package = "envisionR")
  ddin_bold_path <- system.file("fonts", "D-DIN-Bold.ttf",
                                package = "envisionR")
  ddin_italic_path <- system.file("fonts", "D-DIN-Italic.ttf",
                                  package = "envisionR")

  # Getting fonts
  sysfonts::font_add("D-DIN",
                     regular = ddin_regular_path,
                     bold = ddin_bold_path,
                     italic = ddin_italic_path)

  # Running showtext_auto() to load the font.
  showtext::showtext_auto()

  if (!quietly) {
    message("The D-DIN font has been loaded.")
  }
}
