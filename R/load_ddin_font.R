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

  # Getting paths for normal spacing fonts
  ddin_regular_path <- system.file("fonts", "D-DIN.ttf",
                                   package = "envisionR")
  ddin_bold_path <- system.file("fonts", "D-DIN-Bold.ttf",
                                package = "envisionR")
  ddin_italic_path <- system.file("fonts", "D-DIN-Italic.ttf",
                                  package = "envisionR")

  # Getting paths for condensed fonts
  ddincond_regular_path <- system.file("fonts", "D-DINCondensed.ttf",
                                   package = "envisionR")
  ddincond_bold_path <- system.file("fonts", "D-DINCondensed-Bold.ttf",
                                package = "envisionR")

  # Getting paths for expanded fonts
  ddinexp_regular_path <- system.file("fonts", "D-DINExp.ttf",
                                   package = "envisionR")
  ddinexp_bold_path <- system.file("fonts", "D-DINExp-Bold.ttf",
                                package = "envisionR")
  ddinexp_italic_path <- system.file("fonts", "D-DINExp-Italic.ttf",
                                  package = "envisionR")

  # Getting normal spacing fonts
  sysfonts::font_add("D-DIN",
                     regular = ddin_regular_path,
                     bold = ddin_bold_path,
                     italic = ddin_italic_path)

  # Getting condensed fonts
  sysfonts::font_add("D-DIN Condensed",
                     regular = ddincond_regular_path,
                     bold = ddincond_bold_path)

  # Getting expanded fonts
  sysfonts::font_add("D-DIN Expanded",
                     regular = ddinexp_regular_path,
                     bold = ddinexp_bold_path,
                     italic = ddinexp_italic_path)



  # Running showtext_auto() to load the font.
  showtext::showtext_auto()

  if (!quietly) {
    message("The D-DIN font has been loaded.")
  }
}
