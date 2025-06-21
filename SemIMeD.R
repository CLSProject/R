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
    # define dummy data
      # load("./clustern/TCGA_kidney_unnormalized.RData")
      # DATA   <- dataset$data[1:300, 1:100]
      METHOD    <- "euclidean"
      DIAG      <- FALSE
      UPPER     <- TRUE
      P         <-           3
      ALPHA_I   <-         0.5
      ALPHA_J   <-         0.5
      BETA      <-         0.0
      GAMMA     <-        -0.5
      LINK_CRIT <-          ""

  # remove_packages
    pkgs <- names(sessionInfo()$otherPkgs)
    while (length(pkgs) > 0) {
      lapply(pkgs, function(pkg) detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE))
      pkgs <- names(sessionInfo()$otherPkgs)
    }
  # don't install packages as modules should bring there needs with themselves

  # don't load utility scripts besides script to be tested in order not to influence script to be tested
    source("./feature_selection/ExampleFeatureSelection.R")
    source("./distances/Distances.R")
    source("./clustern/clustering_base_algo.R")



# call modules sequentially

  # select data
    cat("\fdistance calculation\n")
    cat("\tin progress\n")
    list <- get_example_data()
    cat("\noutput structure of get_example_data:\n")
    print(str(list))

  readline(prompt = "Press Enter to continue")
  data <- list[[2]]

  # calculate distance matrices and cluster information for both dimensions
    cat("\fDistance calculation\n")
    cat("\tin progress\n")
    dist_duration <- system.time({
      dist_pat <- dist((t(data)), method = METHOD)
      dist_gen <- dist(   data,   method = METHOD)
    })
    cat("\ninput structure of dist(_pat):\n")
    print(str(t(data)))
    cat("\noutput structure of dist(_pat):\n")
    print(str(dist_pat))
    cat("\ninput structure of dist(_gen):\n")
    print(str(data))
    cat("\noutput structure of dist(_gen):\n")
    print(str(dist_gen))
    cat("\nDuration of calculation on patients and on genes: \n")
    print(dist_duration)
    # print(as.matrix(dist_pat)[1:10, 1:5])

  readline(prompt = "Press Enter to continue")
  dist_pat_mat <- as.matrix(dist_pat)
  dist_gen_mat <- as.matrix(dist_gen)

  # calculate cluster informations for both dimensions
    cat("\fCluster calculation\n")
    cat("\tin progress\n")
    clust_duration <- system.time(clust <- cluster_both(dist_pat_mat, dist_gen_mat,
                                                        alpha_i = ALPHA_I, alpha_j = ALPHA_J, beta = BETA, gamma = GAMMA,
                                                        link_crit = LINK_CRIT
                      )
    )
    cat("\ninput structure of cluster_both:\n")
    print(str(as.matrix(dist_pat)))
    cat("\n")
    print(str(as.matrix(dist_gen)))
    cat("\noutput structure of cluster_both:\n")
    print(str(clust))
    cat("\nDuration of calucation on patients and on genes: \n")
    print(clust_duration)
    # plot(clust[[1]])
    plot(as.dendrogram(clust[[1]]))
    plot(as.dendrogram(clust[[2]]))
