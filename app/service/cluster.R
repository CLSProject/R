library(stats)

get_order_dfs <- function(merge) {
  root <- nrow(merge)
  # order <- c()
  dfs <- function(node) {
    for (child in merge[node, ]) {
      if (child < 0) {
        order <- c(order, -child)
      } else {
        order <- c(order, dfs(child))
      }
    }
    order
  }
  dfs(root)
}

# Custom agglomerative hierarchical clustering implementation
# Returns an hclust-compatible object
agglomerative_clustering <- function(
  dist,
  alpha_i = 0.5,
  alpha_j = 0.5,
  beta = 0,
  gamma = 0,
  link_crit = ""
) {
  n <- ncol(dist)
  # Set up initial distance matrix with numeric indices
  colnames(dist) <- rownames(dist) <- as.character(1:n)

  # Each cluster is a vector of its member indices (initially one per observation) # nolint: line_length_linter.
  clusters <- lapply(1:n, function(i) i)
  active <- rep(TRUE, n) # Logical vector: TRUE if cluster is still active

  # Output components for hclust
  # Stores which clusters are merged at each step
  merge <- matrix(0, nrow = n - 1, ncol = 2)
  storage.mode(merge) <- "integer"
  height <- numeric(n - 1) # Stores the height (distance) at each merge

  # Work on a fixed-size distance matrix; set merged clusters to Inf
  dist_mat <- as.matrix(dist)
  diag(dist_mat) <- Inf # Diagonal should not be considered

  cluster_sizes <- rep(1, n) # Track size of each cluster
  cluster_members <- clusters # Track members of each cluster

  # Track cluster labels for hclust merge: negative for leaves, positive for merged clusters # nolint: line_length_linter.
  # First n are leaves, then merged clusters
  cluster_labels <- c(-(1:n), rep(NA, n - 1))

  for (step in 1:(n - 1)) {
    # Find all active clusters
    active_idx <- which(active)
    # Set distances for inactive clusters to Inf so they are ignored
    dist_mat[!active, ] <- Inf
    dist_mat[, !active] <- Inf
    # Find the closest pair of clusters (minimum distance)
    min_idx <- which(dist_mat == min(dist_mat, na.rm = TRUE), arr.ind = TRUE)[1, ] # nolint: line_length_linter.
    ci <- min_idx[1]
    cj <- min_idx[2]
    if (ci > cj) {
      tmp <- ci
      ci <- cj
      cj <- tmp
    } # Ensure ci < cj for consistency
    min_dist <- dist_mat[ci, cj]

    # For hclust: negative for leaves, positive for merged clusters
    merge_ci <- cluster_labels[ci]
    merge_cj <- cluster_labels[cj]
    merge[step, ] <- c(merge_ci, merge_cj)
    height[step] <- min_dist

    # Merge clusters: combine members and update size
    new_members <- c(cluster_members[[ci]], cluster_members[[cj]])
    cluster_members[[ci]] <- new_members
    cluster_sizes[ci] <- length(new_members)
    active[cj] <- FALSE # Deactivate cj (it is now merged)

    # Update cluster_labels: assign new label (step) to ci, NA to cj
    cluster_labels[ci] <- step
    cluster_labels[cj] <- NA

    # Update distances for the new cluster (ci) to all other active clusters
    for (k in active_idx) {
      if (k != ci && active[k]) {
        d_hi <- dist_mat[k, ci] # Distance from k to ci
        d_hj <- dist_mat[k, cj] # Distance from k to cj
        d_ij <- min_dist # Distance between ci and cj
        # UPGMA adjustment if requested
        if (link_crit == "UPGMA") {
          alpha_i <- cluster_sizes[ci] / (cluster_sizes[ci] + cluster_sizes[k])
          alpha_j <- cluster_sizes[k] / (cluster_sizes[ci] + cluster_sizes[k])
        }
        # Lance-Williams formula for updating distances
        d_hk <- alpha_i * d_hi + alpha_j * d_hj + beta * d_ij + gamma * abs(d_hi - d_hj) # nolint: line_length_linter.
        dist_mat[ci, k] <- d_hk
        dist_mat[k, ci] <- d_hk
      }
    }
    # Set distances for merged cluster to Inf so it is ignored
    dist_mat[cj, ] <- Inf
    dist_mat[, cj] <- Inf
  }

  # Build hclust-compatible object
  hc <- list(
    merge = merge, # Merge matrix
    height = height, # Heights at which merges occurred
    order = get_order_dfs(merge), # Order for plotting
    labels = as.character(1:n), # Original labels
    method = link_crit, # Linkage method
    call = match.call(),
    dist.method = "dist_crit"
  )
  class(hc) <- "hclust"
  return(hc)
}

cluster_both <- function(
  dist_pat,
  dist_gene,
  alpha_i,
  alpha_j,
  beta,
  gamma,
  link_crit = ""
) {
  pat_clustering <- agglomerative_clustering(
    dist = dist_pat,
    alpha_i = alpha_i,
    alpha_j = alpha_j,
    beta = beta,
    gamma = gamma,
    link_crit = link_crit
  )
  gene_clustering <- agglomerative_clustering(
    dist = dist_gene,
    alpha_i = alpha_i,
    alpha_j = alpha_j,
    beta = beta,
    gamma = gamma,
    link_crit = link_crit
  )
  return(list(
    pat_clustering = pat_clustering,
    gene_clustering = gene_clustering
  ))
}

calc_params_flexible <- function(beta) {
  alphas <- 1 - beta
  alpha_i <- alpha_j <- alphas / 2
  gamma <- 0
  c(alpha_i = alpha_i, alpha_j = alpha_j, beta = beta, gamma = gamma)
}
