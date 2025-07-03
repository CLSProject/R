z_score_norm <- function(x) {
  (x - mean(x)) / sd(x)
}

min_max_scale_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalise <- function(patient_matrix, normalisation_method) {
  t(apply(patient_matrix, 1, normalisation_method))
}
