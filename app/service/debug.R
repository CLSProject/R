debug_time <- function() {
  Sys.time()
}

time_diff <- function(start_time) {
  cluster_end_time <- Sys.time()
  difftime(cluster_end_time, start_time, units = "secs")
}
