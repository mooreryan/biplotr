# TODO use ggrepel only when actually writing labels.

#' Make the snazziest of biplots!
#'
#' \code{pca_biplot} makes a beautiful biplot of your data, automagically!
#'
#' The \strong{\code{data_cols}} parameter is used to subset the \code{data.frame} or \code{matrix} passed in to the \code{data} parameter.  So, if you passed in \code{1:4}, then the PCA would be calculated using the first four columns of \code{data}.  If you passed in \code{c(2:4, 6)}, then the PCA would be performed on columns 2, 3, 4, and 6 of \code{data}.
#'
#' @export
#'
#' @param data A matrix or data.frame of your data.  Rows will be points, columns will be vectors.
#' @param data_cols A vector of columns to treat as data for the PCA.
#' @param data_projection How do you want to project your data?  Options include "pc_scores" (projections of data into PC space aka principal components) and "pc_scores_scaled" (the same scores, but scaled to unit variance).
#' @param variable_projection How do you want to project your variables?  Options include "loadings" (the variable loadings) and "axes" (the principal axes).
#'
#' @param xaxis_pc Principal component to map to the x-axis.
#' @param yaxis_pc Principal component to map to the y-axis.
#'
#' @param chart_title Title of the chart.
#' @param limits_nudge_x Add this value to both ends of the x-axis (can be negative!).  If the original axis would run from -5 to 5, and you use a value of 1 for this parameter, then the axis will be shown running from -6 to 6 instead.  If you use a -2 here, then the axis will run from -3 to 3 instead of -5 to 5.
#' @param limits_nudge_y Add this value to both ends of the y-axis (can be negative!).  See limits_nudge_x for more info.
#'
#' @param center_data This is an important step for PCA!  Just leave it as \code{TRUE} unless you really know what you're doing.
#' @param scale_data You might want to scale your data if the magnitude of your predictor variables is highly variable, otherwise variables whose magnitude is much larger than the rest of the variables will likely dominate other variables.  On the other hand, this might be what you want.  It's up to you! (Note that you can not use scale if one of your variables is constant or zero.)
#'
#' @param points Do you want to draw the points?
#' @param point_labels Do you want to label the points?
#' @param point_label_size How big do you want the labels?
#' @param point_labels_nudge_y Use this param to specify the amount to nudge the point label away from the point in the y direction.
#'
#' @param arrows Do you want to draw the arrows?
#' @param arrow_labels Do you want to label the arrows?
#' @param arrow_labels_nudge_y How big do you want labels?
#' @param arrow_label_size Use this param to specify the amount to nudge the arrow label away from the tip of the arrow in the y direction.
#' @param arrow_legend Put a snazzy legend to the side instead of labeling individual arrows.
#' @param arrow_scaling_factor Arrows are automatically scaled to fit on the chart based on the data projection.  If you want the arrows longer, pass a value > 1 here.  If you want them to be shorter, pass a value > 0 but < 1.  If you want it to decide automatically, leave it as is (or pass 1).
#'
#' @param use_ggrepel Set this to TRUE if you want to let ggrepel decide where the labels should go.  If you use this option the label nudge options will be ignored.
#'
#' @return A list with attributes biplot and pca.
#'
#' @examples
#' # Using the famous Iris dataset to show a couple of the biplot combinations you can make.
#' library(ggplot2)
#' library(ggrepel)
#' library(grid)
#' library(gridExtra)
#' library(biplotr)
#'
#' dat <- iris[, 1:4]
#'
#' loadings_scores <- pca_biplot(data = dat,
#'                               center_data = TRUE,
#'                               scale_data = FALSE,
#'                               chart_title = "Iris data",
#'                               limits_nudge_y = 0,
#'                               data_projection = "pc_scores",
#'                               variable_projection = "loadings",
#'                               arrow_labels = TRUE)
#'
#' axes_scores <- pca_biplot(data = dat,
#'                           center_data = TRUE,
#'                           scale_data = FALSE,
#'                           chart_title = "Iris data",
#'                           limits_nudge_y = 0,
#'                           data_projection = "pc_scores",
#'                           variable_projection = "axes",
#'                           arrow_labels = TRUE)
#'
#' loadings_scores_scaled <- pca_biplot(data = dat,
#'                                      center_data = TRUE,
#'                                      scale_data = FALSE,
#'                                      chart_title = "Iris data",
#'                                      limits_nudge_y = 0,
#'                                      data_projection = "pc_scores_scaled",
#'                                      variable_projection = "loadings",
#'                                      arrow_labels = TRUE)
#'
#' axes_scores_scaled <- pca_biplot(data = dat,
#'                                  center_data = TRUE,
#'                                  scale_data = FALSE,
#'                                  chart_title = "Iris data",
#'                                  limits_nudge_y = 0,
#'                                  data_projection = "pc_scores_scaled",
#'                                  variable_projection = "axes",
#'                                  arrow_labels = TRUE)
#'
#' grid.arrange(loadings_scores$biplot, axes_scores$biplot,
#'              loadings_scores_scaled$biplot, axes_scores_scaled$biplot,
#'              nrow = 2,
#'              ncol = 2)
#'
#' # Here is a single example of making a biplot from the included team_shooting_mat dataset.
#'
#' library(ggplot2)
#' library(ggrepel)
#' library(grid)
#' library(gridExtra)
#' library(biplotr)
#'
#' # pca_biplot returns a ggplot object
#' chart <- pca_biplot(
#'   # The data matrix
#'   data = team_shooting_mat,
#'
#'   # Add a custom title
#'   chart_title = "NBA Team Shooting 2018",
#'
#'   # Increase the x axis limits by 1 in + and - directions
#'   limits_nudge_x = 1,
#'
#'   # Center the data (important for PCA)
#'   center_data = TRUE,
#'   # Scale the data (since some of our variables have much higher magnitude than others)
#'   scale_data = TRUE,
#'
#'   # Show point labels
#'   point_labels = TRUE,
#'   # Push the labels 0.35 units on the y axis
#'   point_labels_nudge_y = 0.35,
#'
#'   # Show arrow labels
#'   arrow_labels = TRUE,
#'   # Push arrow labels away from arrow heads by 0.35 units
#'   arrow_labels_nudge_y = 0.35
#' )
#'
#' # Draw the plot
#' grid.arrange(chart$biplot)
pca_biplot <- function(data,
                       data_cols = 1:ncol(data),
                       center_data = TRUE,
                       scale_data = FALSE,

                       data_projection = "pc_scores",
                       variable_projection = "loadings",

                       chart_title = FALSE,

                       xaxis_pc = 1,
                       yaxis_pc = 2,

                       limits_nudge_x = 0,
                       limits_nudge_y = 0,

                       points = TRUE,
                       point_labels = FALSE,
                       point_label_size = 3.5,
                       point_labels_nudge_y = 0.5,

                       arrows = TRUE,
                       arrow_labels = FALSE,
                       arrow_labels_nudge_y = 0.5,
                       arrow_label_size = 3.5,
                       arrow_legend = FALSE,
                       arrow_scaling_factor = 1,

                       use_ggrepel = FALSE) {

  ## If we're using ggrepel, ignore the nudge for labels.
  if (use_ggrepel == TRUE) {
    point_labels_nudge_y = 0
    arrow_labels_nudge_y = 0
  }

  if (!is.null(data_projection) && data_projection == "pc_scores_scaled") {
    data_projection <- "pc_scores_scaled"
  } else {
    data_projection <- "pc_scores"
  }

  if (!is.null(variable_projection) && variable_projection == "axes") {
    variable_projection <- "rsvecs"
  } else {
    variable_projection <- "variable_loadings"
  }

  # TODO need to check that the xaxis_pc and yaxis_pc variables are not more than the possible number of PCs

  # Keep only the columns specified as data columns for the pca calculation.
  data_for_pca <- data[, sort(data_cols)]

  # Check that the columns are numeric.
  all_columns_are_numeric <- sapply(
    1:ncol(data_for_pca),
    function(cidx) is.numeric(data_for_pca[, cidx])
  )

    # Give the user a nicer message if they have non numeric columns left after data subsetting.
  stopifnot(all_columns_are_numeric)

  decomp <- biplotr::pca(data = data_for_pca,
                         center_data = center_data,
                         scale_data = scale_data,
                         # We don't need to calculate more singular vectors than this.
                         max_pcs = max(xaxis_pc, yaxis_pc))


  ## For the biplots
  xend <- decomp[[variable_projection]][, xaxis_pc]
  yend <- decomp[[variable_projection]][, yaxis_pc]

  ## We want to nudge the label up
  ## when the arrow is pointing up
  ## and down when the arrow is
  ## pointing down_
  nudge_y <- ifelse(yend >= 0, arrow_labels_nudge_y, -arrow_labels_nudge_y)

  ## We want the arrows scaled so that they are shown nicely with the data projection.
  scaling_factor <- get_scaling_factor(unlist(decomp[[data_projection]]), cbind(xend, yend)) * arrow_scaling_factor

  loadings_df <- data.frame(x = rep(0, times = length(xend)),
                            y = rep(0, times = length(yend)),
                            xend = xend * scaling_factor,
                            yend = yend * scaling_factor,
                            xlabel = xend * scaling_factor,
                            ylabel = yend * scaling_factor + nudge_y,
                            Variable = rownames(decomp[[variable_projection]]))

  xlimits <- get_limits(limits_nudge_x,
                        unlist(decomp[[data_projection]]),
                        loadings_df$xlabel,
                        loadings_df$ylabel)

  ylimits <- get_limits(limits_nudge_y,
                        unlist(decomp[[data_projection]]),
                        loadings_df$xlabel,
                        loadings_df$ylabel)



  ## Now we draw the principal component scores (US) and the
  ## loadings (VS / sqrt(n - 1))

  ## scaling_factor_scores_loadings <-
  ##     scaling_factor(unlist(pc_scores),
  ##                    cbind(loadings_df$xend, loadings_df$yend))

  ## Set default title if needed
  if (chart_title == FALSE) {
    chart_title = "Principal component scores w/Loadings"
  }

  pc_scores_df <- as.data.frame(decomp[[data_projection]])

  # We want the labels to include the amount of variance explained by the desired PC.
  xlabel <- paste("PC",
                  xaxis_pc,
                  " (",
                  round(decomp$pc_variance[xaxis_pc], digits = 1),
                  "%)",
                  sep = "")
  ylabel <- paste("PC",
                  yaxis_pc,
                  " (",
                  round(decomp$pc_variance[yaxis_pc], digits = 1),
                  "%)",
                  sep = "")

  biplot_chart <- ggplot2::ggplot(pc_scores_df,
                                        ggplot2::aes(x = pc_scores_df[, xaxis_pc],
                                                     y = pc_scores_df[, yaxis_pc])) +
    amelia_theme() +
    ggplot2::coord_fixed(xlim = xlimits,
                         ylim = ylimits) +
    ggplot2::ggtitle(chart_title) +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel)

  ## Draw points if we need them.
  if (points == TRUE) {
    biplot_chart <- biplot_chart +
      ggplot2::geom_point()
  }

  ## Draw arrows if we need them.
  if (arrows == TRUE) {
    biplot_chart <- biplot_chart +
      ggplot2::geom_segment(data = loadings_df,
                            mapping = ggplot2::aes(x = x,
                                                   y = y,
                                                   xend = xend,
                                                   yend = yend,
                                                   color = Variable),
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm")),
                            show.legend = arrow_legend)
  }

  ## Add on labels if we need them.
  if (point_labels == TRUE && use_ggrepel == TRUE) {
    biplot_chart <- biplot_chart +
      ggrepel::geom_text_repel(ggplot2::aes(x = PC1,
                                            y = PC2,
                                            label=rownames(decomp[[data_projection]])),
                               vjust = 0.5,
                               size = point_label_size)
  } else if (point_labels == TRUE) {
    biplot_chart <- biplot_chart +
      ggplot2::geom_text(ggplot2::aes(x = PC1,
                                      y = PC2,
                                      label=rownames(decomp[[data_projection]])),
                         vjust = 0.5,
                         nudge_y = point_labels_nudge_y,
                         check_overlap = FALSE,
                         size = point_label_size)
  }

  if (arrow_labels == TRUE && use_ggrepel == TRUE) {
    biplot_chart <- biplot_chart +
      ggrepel::geom_text_repel(data = loadings_df,
                               mapping = ggplot2::aes(x = xlabel,
                                                      y = ylabel,
                                                      label=loadings_df$Variable,
                                                      color = Variable),
                               vjust = 0.5,
                               hjust = 0.5,

                               ## No legend for the arrow labels
                               show.legend = FALSE,
                               size = arrow_label_size)
  } else if (arrow_labels == TRUE) {
    biplot_chart <- biplot_chart +
      ggplot2::geom_text(data = loadings_df,
                         mapping = ggplot2::aes(x = xlabel,
                                                y = ylabel,
                                                label=loadings_df$Variable,
                                                color = Variable),
                         vjust = 0.5,
                         hjust = 0.5,
                         ## nudge_y = arrow_labels_nudge_y,
                         check_overlap = FALSE,

                         ## No legend for the arrow labels
                         show.legend = FALSE,
                         size = arrow_label_size)
  }

  list(biplot = biplot_chart,
       pca = decomp)
}
