remove_packages <- function() {
  remove_all_packages <- function() {
    pkgs <- names(sessionInfo()$otherPkgs)
    while (length(pkgs) > 0) {
      lapply(pkgs, function(pkg) detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE))
      pkgs <- names(sessionInfo()$otherPkgs)
    }
  }
   remove_all_packages()
}

clear_envrionment <- function() {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}
