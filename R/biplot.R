amelia_theme <- function(border_size = 0.5, ...) {
  theme_bw() +
    theme(
      ## Remove the box around the chart
      panel.border = element_blank(),

      ## Remove gridlines
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),

      ## Put back axis lines
      axis.line = element_line(color = "black"),

      ## Adjust border (top, right, bottom, left)
      plot.margin = margin(t = border_size,
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

###### biplot stuff
pca_biplot <- function(data,

                   xaxis_pc = 1,
                   yaxis_pc = 2,

                   chart_title = FALSE,

                   limits_nudge_x = 0,
                   limits_nudge_y = 0,

                   center_data = TRUE,
                   scale_data = FALSE,

                   point_labels = FALSE,
                   point_label_size = 3.5,
                   point_labels_nudge_y = 0.5,

                   arrow_labels = FALSE,
                   arrow_labels_nudge_y = 0.5,
                   arrow_label_size = 3.5,

                   arrow_legend = FALSE) {
  ## First calculate the PCA using SVD

  ## E.g., c("PC1", "PC2", ...)

  ## The number PCs will be the min of the rows and columns of the
  ## data
  num_dims <- min(nrow(data), ncol(data))
  pc_names <- unlist(lapply(1:num_dims,
                            function(n) paste("PC", n, sep = "")))

  ## Center/scale data
  centered_data <- scale(data, center = center_data, scale = scale_data)

  ## SVD
  decomp <- svd(centered_data)

  ## Singular values
  svals <- diag(decomp$d)

  ## Left singular vectors
  lsvecs <- decomp$u
  rownames(lsvecs) <- rownames(data)

  ## Right singular vectors
  rsvecs <- decomp$v
  rownames(rsvecs) <- colnames(data)
  colnames(rsvecs) <- pc_names

  ## TODO output variance explained and cumulative variance in a
  ## nice list.

  ## Eigenvalues show variances of their respective PCs.
  ## Eigenvalues (lambda) are lambda = svals ** 2 / (n - 1), where n
  ## is the number of data
  ## points. (https://stats.stackexchange.com/a/134283)
  variance_explained <- round(decomp$d ** 2 / sum(svals ** 2) * 100,
                              digits = 1)
  cumulative_variance <-  unlist(lapply(1:length(variance_explained),
                                        function(idx) {
                                          sum(variance_explained[1:idx])
                                        }))

  ## Principal component scores.  This is the data projected into
  ## the latent space.
  pc_scores <- lsvecs %*% svals
  rownames(pc_scores) <- rownames(data)
  colnames(pc_scores) <- pc_names

  ## Note that these will be scaled to look good with the plotted
  ## data.  Loadings (len. of arrows ~ SD of orig vars, scalar
  ## products between 2 arrows ~ covariance between them, cosine of
  ## angles between 2 arrows ~ correlations between orig vars)
  loadings <- rsvecs %*% svals / sqrt(nrow(data) - 1)
  rownames(loadings) <- colnames(data)
  colnames(loadings) <- pc_names

  ## For the biplots
  xend <- loadings[, xaxis_pc]
  yend <- loadings[, yaxis_pc]

  ## We want to nudge the label up
  ## when the arrow is pointing up
  ## and down when the arrow is
  ## pointing down_
  nudge_y <- ifelse(yend >= 0, arrow_labels_nudge_y, -arrow_labels_nudge_y)
  scaling_factor <- get_scaling_factor(unlist(pc_scores), cbind(xend, yend))

  loadings_df <- data.frame(x = rep(0, times = length(xend)),
                            y = rep(0, times = length(yend)),
                            xend = xend * scaling_factor,
                            yend = yend * scaling_factor,
                            xlabel = xend * scaling_factor,
                            ylabel = yend * scaling_factor + nudge_y,
                            Variable = rownames(loadings))

  xlimits <- get_limits(limits_nudge_x,
                        unlist(pc_scores),
                        loadings_df$xlabel,
                        loadings_df$ylabel)

  ylimits <- get_limits(limits_nudge_y,
                        unlist(pc_scores),
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

  pc_scores_df <- as.data.frame(pc_scores)
  xlabel <- paste("PC",
                  xaxis_pc,
                  " (",
                  variance_explained[xaxis_pc],
                  "%)",
                  sep = "")
  ylabel <- paste("PC",
                  yaxis_pc,
                  " (",
                  variance_explained[yaxis_pc],
                  "%)",
                  sep = "")

  pc_scores_loadings <- ggplot(pc_scores_df,
                               aes(x = pc_scores_df[, xaxis_pc],
                                   y = pc_scores_df[, yaxis_pc])) +
    amelia_theme() +
    coord_fixed(xlim = xlimits,
                ylim = ylimits) +
    ggtitle(chart_title) +
    xlab(xlabel) +
    ylab(ylabel) +
    geom_point() +
    geom_segment(data = loadings_df,
                 mapping = aes(x = x,
                               y = y,
                               xend = xend,
                               yend = yend,
                               color = Variable),
                 arrow = arrow(length = unit(0.25, "cm")),
                 show.legend = arrow_legend)

  ## Add on labels if we need them.
  if (point_labels == TRUE) {
    pc_scores_loadings <- pc_scores_loadings +
      geom_text(aes(x = PC1,
                    y = PC2,
                    label=rownames(pc_scores)),
                vjust = 0.5,
                nudge_y = point_labels_nudge_y,
                check_overlap = F,
                size = point_label_size)
  }

  if (arrow_labels == TRUE) {
    pc_scores_loadings <- pc_scores_loadings +
      geom_text(data = loadings_df,
                mapping = aes(x = xlabel,
                              y = ylabel,
                              label=loadings_df$Variable,
                              color = Variable),
                vjust = 0.5,
                hjust = 0.5,
                ## nudge_y = arrow_labels_nudge_y,
                check_overlap = T,

                ## No legend for the arrow labels
                show.legend = FALSE,
                size = arrow_label_size)
  }

  ## Return the plot object
  pc_scores_loadings
}
