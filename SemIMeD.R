# prepare SemIMeD run

  # close open devices
    while (dev.cur() > 1) dev.off()

  # clear envrionment
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  # define constants and functions, but be aware they might be overwritten by following loading of scripts
    # create dummy data
      DATA <- matrix(c( 1,  0,  3,  4
                      , 5,  6,  0,  8
                      , 9,  0, 11,  0
                      )
                     ,ncol     = 4
                     ,byrow    = TRUE
                     ,dimnames = list(c("Zeile 1",  "Zeile 2",  "Zeile 3")
                                     ,c("Spalte 1", "Spalte 2", "Spalte 3", "Spalte 4")
                                     )
                     )
    # define dummy parameters
      ALPHA_I   <-         0.5
      ALPHA_J   <-         0.5
      BETA      <-         0.0
      GAMMA     <-         0.0
      DIST_CRIT <- "euclidean"
      LINK_CRIT <-          ""
    # set working directory
      if (!require(this.path)) install.packages("this.path")
      library(this.path)
      setwd(dirname(this.path()))

  # remove_packages
    pkgs <- names(sessionInfo()$otherPkgs)
    while (length(pkgs) > 0) {
      lapply(pkgs, function(pkg) detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE))
      pkgs <- names(sessionInfo()$otherPkgs)
    }
  # don't install packages as modules should bring there needs with themselves



# call sequentially modules
  cat("\f")

  # calculate distance matrixes and cluster informations for both dimensions
    try(detach("package:stats", unload = TRUE), silent = TRUE)
    source("distances/Distances.R")
    source("clustern/clustering_base_algo.R")
    clust <- cluster_both(data = DATA
                         ,alpha_i = ALPHA_I, alpha_j = ALPHA_J, beta = BETA, gamma = GAMMA
                         ,dist_crit = DIST_CRIT, link_crit = LINK_CRIT)

    print(class(clust))
    print(typeof(clust))
    print(str(clust))

    print(class(clust[1]))
    print(typeof(clust[1]))
    print(str(clust[1]))
    plot(clust[1])

    print(class(clust[2]))
    print(typeof(clust[2]))
    print(str(clust[2]))
    plot(as.dendrogram(clust[2]))
