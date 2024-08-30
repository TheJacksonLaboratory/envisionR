#' A \code{ggplot2} template for a faceted violin plot with activity in light and dark
#'
#' This function returns a template \code{ggplot2} object that is a violin plot with
#'  activity in the light and dark.
#' @param activity_data activity data frame.
#' @param metadata a metadata object as made by \code{envision_metadata()}.
#' @param visualize_on the variable to visualize on for colors,
#'  a \code{character} of length 1 (defaults to \code{group_name}).
#'  NOTE: as of 2024-08-30, the ability to visualize on grouping variables
#'  other than \code{group_name} or \code{minoccupancy} has not been implemented.
#' @param yvar the variable for the y-axis,
#'  a \code{character} of length 1.
#' @param colors user-defined colors for the plot.
#' @param y_axis_label user-specified y-axis label.
#' @param quietly should the function suppress warnings? (default: \code{FALSE})
#' @returns a \code{ggplot2} based violin plot with inset box-and-whiskers.
#' @keywords Envision
#' @examples
#' # Making dummy metadata
#' metadata <- envision_metadata(
#'   study_name = "A",
#'   tzone = "US/Pacific",
#'   lights_on = "06:00:00",
#'   lights_off = "18:00:00",
#'   study_url = "https://envision.jax.org/org/1001/study/1002/overview"
#' )
#'
#' # Getting dummy dataset
#' data("activity_cage_data_example")
#'
#' # Plotting dummy data
#' lightdark_violin_plot(
#'   activity_data = activity_cage_data_example,
#'   metadata = metadata,
#'   visualize_on = "group_name",
#'   yvar = "movement_mean_per_cage_cm_s_hour"
#' )
#' @export
lightdark_violin_plot <- function(activity_data,
                                  metadata = NULL,
                                  visualize_on,
                                  yvar,
                                  colors = NULL,
                                  y_axis_label = NULL,
                                  quietly = FALSE) {
  stopifnot(requireNamespace("ggplot2"))
  stopifnot(requireNamespace("tibble"))
  stopifnot(requireNamespace("dplyr"))
  stopifnot(requireNamespace("scales"))

  # binding global variables
  cage_name <- group_name <- animals_cage_quantity <- visualize <-
    var_col <- NULL

  stopifnot("data is not a tibble" = tibble::is_tibble(activity_data))
  stopifnot(
    "the column title in yvar is not in activity_data" =
      yvar %in% colnames(activity_data)
  )
  stopifnot(
    "visualze_on must be either minoccupancy or group_name" =
      visualize_on %in% c("minoccupancy", "group_name")
  )

  if (visualize_on == "minoccupancy") {
    activity_data <- activity_data |>
      dplyr::group_by(cage_name) |>
      dplyr::mutate(
        animals_cage_quantity = ifelse(animals_cage_quantity == 0,
          NA,
          animals_cage_quantity
        ),
        visualize = min(animals_cage_quantity, na.rm = TRUE),
        visualize = paste(visualize, ifelse(visualize == 1,
          "animal",
          "animals"
        ))
      )
    legendtitle <- "Min Cage Occupancy"
  } else if (visualize_on == "group_name") {
    activity_data <- activity_data |>
      dplyr::group_by(cage_name) |>
      dplyr::mutate(visualize = group_name)
    legendtitle <- "Group"
  }
  activity_data[, "var_col"] <- activity_data[, yvar]

  if (is.null(colors)) {
    plotcolors <- scales::hue_pal()(length(unique(dplyr::pull(activity_data, "visualize"))))
  } else {
    if (length(colors) < length(unique(dplyr::pull(activity_data, "visualize")))) {
      stop("insufficient number of colors provided")
    } else if (length(colors) > length(unique(dplyr::pull(activity_data, "visualize"))) & !quietly) {
      warning("more colors provided than number of grouping variables")
      plotcolors <- colors[seq_len(length(unique(dplyr::pull(activity_data, "visualize"))))]
    } else {
      plotcolors <- colors
    }
  }

  if (!is.null(metadata)) {
    if (metadata[["study_name"]] != "") {
      plot_title <- paste0(metadata[["study_name"]])
    } else {
      plot_title <- NULL
    }
  } else {
    plot_title <- NULL
  }

  if (is.null(y_axis_label)) {
    y_axis_label <- yvar
  }

  violinplot <- ggplot2::ggplot(
    data = activity_data,
    ggplot2::aes(
      x = cage_name,
      y = var_col,
      color = visualize
    )
  ) +
    ggplot2::geom_violin(fill = "#FFFFFF") +
    ggplot2::geom_boxplot(width = 0.1, fill = "#FFFFFF", outlier.color = NA) +
    ggplot2::facet_wrap(. ~ light_cycle,
      nrow = 2,
      scales = "free_y", as.table = FALSE
    ) +
    ggplot2::scale_color_manual(
      name = legendtitle,
      values = plotcolors
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "#FFFFFF"),
      legend.position = "bottom"
    ) +
    ggplot2::xlab("Cage") +
    ggplot2::ylab(y_axis_label) +
    ggplot2::ggtitle(plot_title)

  return(violinplot)
}
