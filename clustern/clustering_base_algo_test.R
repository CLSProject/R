# prepare tests

  # close open devices
    while (dev.cur() > 1) dev.off()

  # clear envrionment
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)

  # don't install packages besides test suite in order not to influence script to be tested

  # define constants and functions, but be aware they might be overwritten by following loading of scripts
    load("TCGA_kidney_unnormalized.RData")
    data   <- dataset$data[1:300, 1:100]
    if (!require(stats)) install.packages("stats")
    library(stats)
    STATS_DIST <- dist(data)
    # remove_packages
      pkgs <- names(sessionInfo()$otherPkgs)
      while (length(pkgs) > 0) {
        lapply(pkgs, function(pkg) detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE))
        pkgs <- names(sessionInfo()$otherPkgs)
      }
    METHOD <- "euclidean"
    DIAG   <- FALSE
    UPPER  <- TRUE
    P      <-           3

  # don't load utility scripts besides script to be tested in order not to influence script to be tested



# execute test

  # cluster by stats package
    cat("\fcluster by stats package\n")
    if (!require(stats)) install.packages("stats")
    library(stats)
    stats_clust <- hclust(STATS_DIST)
    print(str(stats_clust))
    plot(as.dendrogram(stats_clust))
    plot(stats_clust)
    cat("\n")

  # cluster by script
    cat("\n\n\ncluster by clustering base algo script\n")
    source("clustering_base_algo.R")
    script_clust <- agglomerative_clustering_base_algo(as.matrix(STATS_DIST)
                                                      ,alpha_i=0.5,alpha_j=0.5,beta=0,gamma=0.5
                                                      ,link_crit=""
                                                      )
    print(str(script_clust))
    plot(as.dendrogram(script_clust))
    plot(script_clust)
    cat("\n")

  # compare results
    cat("\n\n\n")
    print(all.equal(stats_clust, script_clust))
    cat("\n")
    print(identical(stats_clust, script_clust))
