# TODO use ggrepel only when actually writing labels.

# Helpers

"%nin%" <- Negate("%in%")


#' Make the snazziest of biplots!
#'
#' \code{pca_biplot} makes a beautiful biplot of your data!
#'
#' The \strong{\code{data_cols}} parameter is used to subset the \code{data.frame} or \code{matrix} passed in to the \code{data} parameter.  So, if you passed in \code{1:4}, then the PCA would be calculated using the first four columns of \code{data}.  If you passed in \code{c(2:4, 6)}, then the PCA would be performed on columns 2, 3, 4, and 6 of \code{data}.
#'
#' The \strong{\code{scale_data}} parameter is used to scale the data before running PCA.  You might want to scale your data if the magnitude of your predictor variables is highly variable, otherwise variables whose magnitude is much larger than the rest of the variables will likely dominate other variables.  On the other hand, this might be what you want.  It's up to you! (Note that you can not use scale if one of your variables is constant or zero.)
#'
#' The \strong{\code{point_color}} option is used to color the points by some column of the \code{data}.  It is similar to the \code{color} parameter of the \code{aes} function in \code{ggplot}, except that it must be provided as a string/character rather than a Symbol.  E.g., if you want to color the points by the \code{Species} column, use \code{point_color = "Species"} rather than \code{point_color = Species}.  If \strong{\code{point_color}} is also present in the columns specified by \code{data_cols}, it will be removed from the \code{data_cols}.
#'
#' The \strong{\code{use_ggrepel}} parameter is used to enable \code{ggrepel} to decide where the labels should be automatically.  This works really well for the points (unless there are a lot of them in which case it is SLOW), but not as well for the arrows.  It technically treats the tip of the arrow as the point to avoid, so it will often collide with the line part of the arrow.
#'
#' The \strong{return value} is a \code{list} with attributes \code{biplot} and \code{pca}.
#'
#' The \strong{\code{biplot}} attribute contains the ggplot object.  It's just like anyother ggplot object, so, depending on how you originally called the \code{pca_biplot} function, you could customize it more to your liking (see examples).
#'
#' The \strong{\code{pca}} attribute contains all the PCA data wrapped in a \code{list}.  This contains all you need to make whichever kind of biplots you like or to create your own custom biplot chart.  It's attributes are:
#' \itemize{
#'   \item \emph{svals}: singular values
#'   \item \emph{lsvecs}: left singular vectors
#'   \item \emph{rsvec}: right singular vectors
#'   \item \emph{pc_variance}: variance explained by the included PCs
#'   \item \emph{cum_var}: cumulative variance explained
#'   \item \emph{pc_scores}: matrix with the PC scores
#'   \item \emph{pc_scores_scaled}: matrix with the PC scores scaled to unit variance
#'   \item \emph{variable_loadings}: matrix with the variable loadings
#' }
#'
#' The \strong{\code{plot_elem}} attribute contains a list with a bunch of things to help the user customize the plot.
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
#' @param scale_data Do you want to scale the data before doing PCA?
#'
#' @param points Do you want to draw the points?
#' @param point_color Column name (as a string) to color the points by.
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
#' @return A list with attributes \code{biplot}, \code{pca}, and \code{plot_elem}.  (See Details for more info.)
#'
#' @examples
#' # Import needed libraries.
#' library(ggplot2)
#' library(ggrepel)
#' library(grid)
#' library(gridExtra)
#' library(biplotr)
#'
#' #### Here is a simple example of making a biplot using the iris dataset included in R.
#'
#' chart <- pca_biplot(iris,
#'                     # Show the arrows
#'                     arrows = TRUE,
#'
#'                     # Print the arrow labels
#'                     arrow_labels = TRUE,
#'
#'                     # Sometimes the arrow labels are too close or too far from the arrow tips.
#'                     # This moves the arrow labels out from the tips by 0.3 units in up or
#'                     # down in the y-axis direction.
#'                     arrow_labels_nudge_y = 0.3,
#'
#'                     # Color the points by the Species column.
#'                     point_color = "Species",
#'
#'                     # Increase the x-axis limits a bit so the arrow labels don't get cut off.
#'                     limits_nudge_x = 1,
#'
#'                     # Decrease the y-axis limits a bit as by default, it makes a square chart,
#'                     # but this data is not too spread out in the y-axis.
#'                     limits_nudge_y = -1)
#'
#' # Show the plot!
#' print(chart$biplot)
#'
#' #### Here is an example showing that the biplot attr of the return list is just
#' #### an ordinary ggplot object.
#'
#' chart <- pca_biplot(iris, data_cols = 1:4, points = FALSE, arrows = FALSE)
#'
#' # Print a chart with purple points.
#' print(chart$biplot + geom_point(color = "#aa2288"))
#'
#' #### This example shows that you could use the pca attr of the return list to make your own plot.
#'
#' chart <- pca_biplot(iris, data_cols = 1:4)
#'
#' plot(chart$pca$pc_scores)
#'
#' #### This example shows that you must specify data_cols if your input data has lots of extra
#' #### variables not meant to be in the PCA. (See the Vignettes for more info.)
#'
#' iris2 <- cbind(
#'   iris,
#'   Group = c(
#'     rep(1, times = nrow(iris) / 2),
#'     rep(2, times = nrow(iris) / 2)
#'   )
#' )
#'
#' chart <- pca_biplot(iris2, data_cols = 1:4, point_color = "Species")
#'
#' #### This example shows how to use the items in the plot_elem return attr to build up your own
#' #### custom plots.
#'
#' # First do the PCA
#' p <- pca_biplot(iris, point_color = "Species")
#'
#' # This will print the normal biplot.
#' print(p$biplot)
#'
#' # And this is more or less a recreation of the biplot contained in p$biplot using the items
#' # in p$plot_elem.
#' p$plot_elem$biplot_chart_base +
#'   biplotr::theme_amelia() +
#'   geom_point(aes(color = Species)) +
#'   geom_segment(data = p$plot_elem$loadings_df,
#'                mapping = aes(x = x, y = y, xend = xend, yend = yend),
#'                arrow = arrow(length = unit(0.25, "cm")))
#'
#'
#' #### This example shows the different types of biplots you can make.
#'
#' loadings_scores <- pca_biplot(data = iris,
#'                               # Color points by "Species" column.
#'                               point_color = "Species",
#'                               arrow_labels = TRUE,
#'                               data_projection = "pc_scores",
#'                               variable_projection = "loadings")
#'
#' axes_scores <- pca_biplot(data = iris,
#'                           # Color points by "Species" column.
#'                           point_color = "Species",
#'                           arrow_labels = TRUE,
#'                           data_projection = "pc_scores",
#'                           variable_projection = "axes")
#'
#' loadings_scores_scaled <- pca_biplot(data = iris,
#'                                      # Color points by "Species" column.
#'                                      point_color = "Species",
#'                                      arrow_labels = TRUE,
#'                                      data_projection = "pc_scores_scaled",
#'                                      variable_projection = "loadings")
#'
#' axes_scores_scaled <- pca_biplot(data = iris,
#'                                  # Color points by "Species" column.
#'                                  point_color = "Species",
#'                                  arrow_labels = TRUE,
#'                                  data_projection = "pc_scores_scaled",
#'                                  variable_projection = "axes")
#'
#' # Note that the ggplot object is in the $biplot attribute.
#' grid.arrange(loadings_scores$biplot, axes_scores$biplot,
#'              loadings_scores_scaled$biplot, axes_scores_scaled$biplot,
#'              nrow = 2,
#'              ncol = 2)
#'
#' #### Here is an example of making a biplot from the included team_shooting_mat dataset.
#' #### It includes examples for customizing the look of the chart.
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
                       point_color = NULL,
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




  if (is.null(point_color)) {
    keep_these_cols <- sort(data_cols)
  } else {
    # First things first, check and make sure that column actually exists.
    if (point_color %nin% colnames(data)) {
      stop("The specified point_color column was not present in the data!")
    }

    # Which column index matches the point_color column name?
    grouping_col_idx <- which(colnames(data) == point_color)

    # Then get the columns to keep.
    keep_these_cols <- sort(setdiff(data_cols, grouping_col_idx))
  }
  data_for_pca <- data[, keep_these_cols]

  # Check that the columns are numeric.
  all_columns_are_numeric <- sapply(
    1:ncol(data_for_pca),
    # Need to unlist here in case input data is a tibble.
    function(cidx) is.numeric(unlist(data_for_pca[, cidx]))
  )

    # Give the user a nicer message if they have non numeric columns left after data subsetting.
  if (!all(all_columns_are_numeric)) {
    stop(paste0(
      "All columns used for PCA are not numeric!  Make sure to select only numeric columns for PCA! (cols included in PCA were: '",
                paste(keep_these_cols, collapse = ", "),
                "')"
      ))
  }

  decomp <- biplotr::pca(data = data_for_pca,
                         center_data = center_data,
                         scale_data = scale_data,
                         # We don't need to calculate more singular vectors than this.  Always include at least 3 PCs for easy 3d biplots down the line.
                         max_pcs = max(xaxis_pc, yaxis_pc, 3))


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

  # Merge the original data with the pc_scores_df so it is easier to color by lots of different things.

  # if (!is.null(point_color)) {
  #   pc_scores_df[[point_color]] <- data[[point_color]]
  # }

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

  # First make a df for the "covariates" (or whatever is in there).  This is all the data not included in the PCA.  We wrap it in a data.frame in case there is only one covariate column.
  covariate_df <- as.data.frame(data[, -keep_these_cols])

  # Then add in the proper column names.
  colnames(covariate_df) <- colnames(data)[-keep_these_cols]

  data_for_ggplot <- cbind(pc_scores_df, covariate_df)

  plot_elem <- list(data_for_ggplot = data_for_ggplot)

  biplot_chart <- ggplot2::ggplot(data = data_for_ggplot,
                                  mapping = ggplot2::aes(x = pc_scores_df[, xaxis_pc],
                                                         y = pc_scores_df[, yaxis_pc])) +
    ggplot2::coord_fixed(xlim = xlimits,
                         ylim = ylimits) +
    ggplot2::ggtitle(chart_title) +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel)

  # Save a copy of the undecorated biplot chart and return it so the user can customize.
  plot_elem$biplot_chart_base <- biplot_chart

  # Now add the theme
  biplot_chart <- biplot_chart + theme_amelia()

  ## Draw points if we need them.
  if (points == TRUE && is.null(point_color)) {
    biplot_chart <- biplot_chart +
      ggplot2::geom_point()
  } else if (points == TRUE) {
    biplot_chart <- biplot_chart +
      ggplot2::geom_point(ggplot2::aes(color = !!rlang::sym(point_color)))
  }

  ## Draw arrows if we need them.
  if (arrows == TRUE) {
    biplot_chart <- biplot_chart +
      ggplot2::geom_segment(data = loadings_df,
                            mapping = ggplot2::aes(x = x,
                                                   y = y,
                                                   xend = xend,
                                                   yend = yend),
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
                                                      label = Variable),
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
                                                label = Variable),
                         vjust = 0.5,
                         hjust = 0.5,
                         ## nudge_y = arrow_labels_nudge_y,
                         check_overlap = FALSE,

                         ## No legend for the arrow labels
                         show.legend = FALSE,
                         size = arrow_label_size)
  }

  plot_elem$loadings_df <- loadings_df
  plot_elem$xlimits <- xlimits
  plot_elem$ylimits <- ylimits
  plot_elem$xlabel <- xlabel
  plot_elem$ylabel <- ylabel

  list(biplot = biplot_chart,
       pca = decomp,
       plot_elem = plot_elem)
}
