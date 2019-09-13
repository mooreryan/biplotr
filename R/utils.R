# Some utility function!

#' The \code{pca_biplot} ggplot2 theme
#'
#' This is the theme used for \code{pca_biplot} charts.
#'
#' @export
theme_amelia <- function(border_size = 0.5, ...) {
  ggplot2::theme_bw() +
    ggplot2::theme(
      ## Remove the box around the chart
      panel.border = ggplot2::element_blank(),

      ## Remove gridlines
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),

      ## Put back axis lines
      axis.line = ggplot2::element_line(color = "black"),

      ## Adjust border (top, right, bottom, left)
      plot.margin = ggplot2::margin(t = border_size,
                                    r = border_size,
                                    b = border_size,
                                    l = border_size,
                                    unit = "cm"),

      ## The rest of the args
      ...
    )
}

get_largest_magnitude <- function(vec) {
  max(apply(vec, 2, FUN = function(x) { max(abs(x)) }))
}

## Given the points and the vectors, we want to make sure the vectors
## are on the same scale as to fit nicely with the points.
get_scaling_factor <- function(points, vectors) {
  get_largest_magnitude(points) / get_largest_magnitude(vectors)
}

## Given some data sets, get the overall min and max to have nice
## square plots.
get_limits <- function(nudge, ...) {
  ## Always include the origin
  all_points <- unlist(c(0, ...))

  c(min(all_points) - nudge, max(all_points) + nudge)
}
