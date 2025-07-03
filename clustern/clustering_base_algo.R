

#after fusion of clusters is completed, retrieve order of datapoints via dfs
getOrder_dfs<-function(merge){
  root<-nrow(merge)
  order<-c()
  dfs<-function(node){
    for (child in merge[node,]){
      if (child <0){# if original cluster
         order<-c(order,-child)#add absolute value of cluster to order
      }
      else  order<-c(order,dfs(child))#otherwise visit child
    }
    return (order)
  }
  dfs(root)
}

#base algo for aggolmerative clustering, can work with diffrent linkage criteria, dist is precalculated 
agglomerative_clustering_base_algo<-function(dist,alpha_i=0.5,alpha_j=0.5,beta=0,gamma=0,link_crit=""){
    n<-ncol(dist)
    colnames(dist)<-as.character(-(1:n))#to get cluster distances by names from dist
    rownames(dist)<-as.character(-(1:n))
    clusters <- lapply(1:n, function(i) -i)
    names(clusters)<-as.character(-(1:n))#negative clusternames for initial clusters

    merge<-matrix(0,nrow = n-1,ncol=2)
    storage.mode(merge)<-"integer"
    height<-numeric(n-1)

    cluster_id_counter<-1 #fused cluster ids start at 1

    while(length(clusters)>1){# While more than one cluster exists
      min_dist<-Inf
      pair<-c(NA,NA)
      cluster_names<-names(clusters)

      for (i in 1:(length(clusters)-1)){ #search cluster matrix for minimal distance
        for (j in(i+1): length(clusters)){
          ci_name <- names(clusters)[i]
          cj_name <- names(clusters)[j]

          dists<-dist[ci_name,cj_name,drop=FALSE]
          link_dist<-min(dists)

          if (link_dist<min_dist){
            min_dist<-link_dist
            pair<-c(ci_name,cj_name)
          }

        }
      }
      ci_name <- pair[1] #Clusters to fuse 
      cj_name <-pair[2]
      ci_points <- clusters[[ci_name]]# data points already in cluster
      cj_points <- clusters[[cj_name]]
      new_cluster_points <- c(ci_points, cj_points)

      new_cluster_name <- as.character(cluster_id_counter)


      merge[cluster_id_counter, ] <- c(as.integer(ci_name), as.integer(cj_name))#Update merge matrix
      height[cluster_id_counter] <- min_dist# update height vector

      cluster_i_size=length(clusters[[ci_name]])#this is needed for UPGMA
      cluster_j_size=length(clusters[[cj_name]])

      clusters_to_remove <- sort(pair, decreasing = TRUE)
      clusters[[clusters_to_remove[1]]] <- NULL
      clusters[[clusters_to_remove[2]]] <- NULL


      not_fused_clusters<-clusters      #h: clusters not fused in this step
      clusters[[as.character(cluster_id_counter)]]<-new_cluster_points
      #calculdate new inter cluster distances
      new_distances<-numeric(length(not_fused_clusters))
      k<-new_cluster_name
      if (link_crit=="UPGMA"){
        alpha_i=cluster_i_size/(cluster_i_size+cluster_j_size)#calculate new params for UPGMA
        alpha_j=cluster_j_size/(cluster_i_size+cluster_j_size)

      }
      if (length(not_fused_clusters)>0){
        for (i in 1:length(not_fused_clusters)){

          h<-as.integer(not_fused_clusters[[i]])# one of the clusters that was not fused
          h_name<-names(not_fused_clusters)[i]
          d_hi<-dist[h_name,ci_name]#distance between not fused cluster h and fused cluster i
          d_hj<-dist[h_name,cj_name]#same thing for cluster j
          d_ij<-min_dist#distance between i and j
          d_hk<-alpha_i*d_hi+alpha_j*d_hj+beta*d_ij+gamma*abs(d_hi-d_hj)#Lance Williams formula
          new_distances[i]<-d_hk
        }
        # remove olds stuff from distance matrix
        dist <- dist[!(rownames(dist) %in% pair), !(colnames(dist) %in% pair)]
        #update dist matrix
        names(new_distances)<-names(not_fused_clusters)
        new_dists<-matrix(0,nrow=length(clusters),ncol=length(clusters))
        colnames(new_dists)<-names(clusters)
        rownames(new_dists)<-names(clusters)
        new_dists[k, names(new_distances)] <- new_distances
        new_dists[names(new_distances), k] <- new_distances
        if (is.matrix(dist) && nrow(dist) > 0 && ncol(dist) > 0){
          new_dists[rownames(dist),colnames(dist)]<-dist[rownames(dist),colnames(dist)]

        }


        dist<- new_dists

        cluster_id_counter <- cluster_id_counter + 1# increase counter for next cluster
      }
    }



  #mimic hclust structure
  hc <- list(
    merge = merge,
    height = height,
    order = getOrder_dfs(merge),
    labels = "labels",#Dummy 
    method = link_crit,
    call = match.call(),
    dist.method = "dist_crit"#Dummy
  )

  class(hc) <- "hclust" #make it look like hclust object
  return(hc)
}
#clusters both directions and returns list for visualization
#gets precalculated distance matrix from distances 
cluster_both<-function(dist_pat,dist_gene,alpha_i,alpha_j,beta,gamma,link_crit=""){
  pat_clustering=agglomerative_clustering_base_algo(dist=dist_pat,alpha_i=alpha_i,alpha_j=alpha_j,beta=beta,gamma=gamma,link_crit=link_crit)
  gene_clustering=agglomerative_clustering_base_algo(dist=dist_gene,alpha_i=alpha_i,alpha_j=alpha_j,beta=beta,gamma=gamma,link_crit=link_crit)
  return(list(
    pat_clustering = pat_clustering,
    gene_clustering = gene_clustering
  ))

}

#can calculate other parameters based on beta for flexible linkage strategy
#this is not called anymore..was moved into GUI
calc_params_flexible<-function(beta){
  alphas<-1-beta
  alpha_i=alpha_j=alphas/2
  gamma=0
  return(c(alpha_i = alpha_i, alpha_j = alpha_j, beta = beta, gamma = gamma))
}

