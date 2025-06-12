# prepare SemIMeD run

  # close open devices
    while (dev.cur() > 1) dev.off()

  # clear envrionment
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  # define constants and functions, but be aware they might be overwritten by following loading of scripts
    # set working directory
      if (!require(this.path)) install.packages("this.path")
      library(this.path)
      setwd(dirname(this.path()))
    # create dummy data
      load("./clustern/TCGA_kidney_unnormalized.RData")
      DATA   <- dataset$data[1:300, 1:100]
      if (!require(stats)) install.packages("stats")
      library(stats)
      METHOD <- "euclidean"
      DIAG   <- FALSE
      UPPER  <- TRUE
      P      <-           3
    # define dummy parameters
      ALPHA_I   <-         0.5
      ALPHA_J   <-         0.5
      BETA      <-         0.0
      GAMMA     <-         0.0
      DIST_CRIT <- "euclidean"
      LINK_CRIT <-          ""

  # remove_packages
    pkgs <- names(sessionInfo()$otherPkgs)
    while (length(pkgs) > 0) {
      lapply(pkgs, function(pkg) detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE))
      pkgs <- names(sessionInfo()$otherPkgs)
    }
  # don't install packages as modules should bring there needs with themselves

  # don't load utility scripts besides script to be tested in order not to influence script to be tested




# call sequentially modules
  cat("\f")

  # calculate distance matrixes and cluster informations for both dimensions
    source("./distances/Distances.R")
    dist_duration <- system.time({
      dist_pat  <- as.matrix(dist((t(DATA)), method = METHOD))
      dist_gene <- as.matrix(dist(   DATA,   method = METHOD))
    })
    print(dist_duration)
  # calculate cluster informations for both dimensions
    source("./clustern/clustering_base_algo.R")
    clust_duration <- system.time(clust <- cluster_both(dist_pat, dist_gene
                                                       ,alpha_i = ALPHA_I, alpha_j = ALPHA_J, beta = BETA, gamma = GAMMA
                                                       ,link_crit = LINK_CRIT
                                                       )
                                 )
    print(clust_duration)
    # plot(clust[[1]])
    plot(as.dendrogram(clust[[2]]))
