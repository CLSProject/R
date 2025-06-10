clear_envrionment <- function() {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}

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

print_properties <- function(obj) {
  print(class(obj))
  print(typeof(obj))
  print(dim(obj))
  print(attributes(obj))
}
