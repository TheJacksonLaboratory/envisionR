#' @export
lightdark_violin_plot <- function(activity_data,
                                  metadata,
                                  visualize_on = NULL,
                                  yvar = NULL,
                                  colors = NULL,
                                  y_axis_label = NULL,
                                  quietly = FALSE) {
  stopifnot(requireNamespace("ggplot2"))
  stopifnot(requireNamespace("tibble"))
  stopifnot(requireNamespace("dplyr"))
  stopifnot(requireNamespace("scales"))

  # binding global variables
  cage_name <- animals_cage_quantity <- visualize <- group <- var_col <- NULL


  stopifnot("data is not a tibble" = tibble::is_tibble(activity_data))
  stopifnot(
    "the column title in yvar is not in activity_data" =
      yvar %in% colnames(activity_data)
  )
  stopifnot(
    "visualze_on must be either minoccupancy or group" =
      visualize_on %in% c("minoccupancy", "group")
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
  } else if (visualize_on == "group") {
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
    } else {
      plotcolors <- colors
    }
  }
  if (!is.null(metadata) & metadata[["study_name"]] != "") {
    plot_title <- paste0(metadata[["study_name"]])
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
