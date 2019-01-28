#' Principal components analysis (PCA) calculated using singular value decomposition (SVD).
#'
#' @export
#'
#' @param center_data This is an important step for PCA!  Just leave it as \code{TRUE} unless you really know what you're doing.
#' @param scale_data You might want to scale your data if the magnitude of your predictor variables is highly variable, otherwise variables whose magnitude is much larger than the rest of the variables will likely dominate other variables.  On the other hand, this might be what you want.  It's up to you! (Note that you can not use scale if one of your variables is constant or zero.)
#' @param max_pcs If you have a large matrix that is taking a lot of time, but you only need to keep a couple of PCs, you could set max_pcs to something like 2 (e.g., if you only need to make a biplot) in order to speed up the computation.
pca <- function(data,
                center_data = TRUE,
                scale_data = TRUE,
                max_pcs = NULL) {

  # Max PCs needs to be checked here in case user wants fewer than total PCs.
  num_pcs <- min(nrow(data), ncol(data), max_pcs)

  # E.g., c("PC1", "PC2", ...)
  pc_names <- unlist(lapply(1:num_pcs,
                            function(n) paste("PC", n, sep = "")))

  # Center/scale data
  centered_data <- scale(data,
                         center = center_data,
                         scale = scale_data)

  # SVD.  Only calculate the required number of PCs.
  decomp <- svd(centered_data, nu = num_pcs, nv = num_pcs)

  # Singular values.
  svals <- decomp$d

  # TODO assert that ncol(decomp$u) == ncol(decomp$v)
  # svd() will always calculate all singular values.  We may need less for matrix multiplication.
  svals_for_mat_mul <- diag(decomp$d[1:ncol(decomp$u)])

  # Left singular vectors
  lsvecs <- decomp$u
  rownames(lsvecs) <- rownames(data)
  colnames(lsvecs) <- pc_names

  # Right singular vectors
  rsvecs <- decomp$v
  rownames(rsvecs) <- colnames(data)
  colnames(rsvecs) <- pc_names

  # Eigenvalues show variances of their respective PCs.
  # Eigenvalues (lambda) are lambda = svals ** 2
  # (https://stats.stackexchange.com/a/134283)

  # Variance explained for each PC
  pc_variance <- svals ** 2 / sum(svals ** 2) * 100

  # Cumulative variance as you add more and more PCs
  cum_var <-  cumsum(pc_variance)

  # Principal component scores, i.e., the rows projected into latent PC space.
  pc_scores <- lsvecs %*% svals_for_mat_mul
  rownames(pc_scores) <- rownames(data)
  colnames(pc_scores) <- pc_names

  # Scaled to unit variance
  pc_scores_scaled <- lsvecs * sqrt(nrow(data) - 1)
  rownames(pc_scores_scaled) <- rownames(data)
  colnames(pc_scores_scaled) <- pc_names

    # Note that in the biplots, these will be scaled to look good with the plotted
  # data.  Loadings (len. of arrows ~ SD of orig vars, scalar
  # products between 2 arrows ~ covariance between them, cosine of
  # angles between 2 arrows ~ correlations between orig vars)
  variable_loadings <- rsvecs %*% svals_for_mat_mul / sqrt(nrow(data) - 1)
  rownames(variable_loadings) <- colnames(data)
  colnames(variable_loadings) <- pc_names

  list(svals = svals,
       lsvecs = lsvecs,
       rsvecs = rsvecs,
       pc_variance = pc_variance,
       cum_var = cum_var,
       pc_scores = pc_scores,
       pc_scores_scaled = pc_scores_scaled,
       variable_loadings = variable_loadings)
}
