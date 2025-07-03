###
# different normalization functions
z_score_norm <- function(x) {
  (x - mean(x)) / sd(x)
}


min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
