#Test funktioniert so weit, noch zu formalisieren: Input und Output Parameter, wo werden diese abgelegt, wer muss zugreifen?
#Sollte nach Integration nochmal genau getestet werden, insb. Aufrufen der Daten - werden diese von Lucy auch als RData Format abgelegt?
#Falls ja, wo, Pfad sollte definiert werden um abrufbar zu sein

library(stats)
getOrder_dfs<-function(merge){
  root<-nrow(merge)
  order<-c()
  dfs<-function(node){
    for (child in merge[node,]){
      if (child <0){
         order<-c(order,-child)
      }
      else  order<-c(order,dfs(child))
    }
    return (order)
  }
  dfs(root)
}


agglomerative_clustering_base_algo<-function(dist,alpha_i=0.5,alpha_j=0.5,beta=0,gamma=0,link_crit=""){
    n<-ncol(dist)
    colnames(dist)<-as.character(-(1:n))
    rownames(dist)<-as.character(-(1:n))
    clusters <- lapply(1:n, function(i) -i)
    names(clusters)<-as.character(-(1:n))
    #labels<-colnames(data)

    merge<-matrix(0,nrow = n-1,ncol=2)
    storage.mode(merge)<-"integer"
    height<-numeric(n-1)

    cluster_id_counter<-1

    while(length(clusters)>1){
      min_dist<-Inf
      pair<-c(NA,NA)
      cluster_names<-names(clusters)

      for (i in 1:(length(clusters)-1)){
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
      ci_name <- pair[1]
      cj_name <-pair[2]
      ci_points <- clusters[[ci_name]]
      cj_points <- clusters[[cj_name]]
      new_cluster_points <- c(ci_points, cj_points)

      new_cluster_name <- as.character(cluster_id_counter)


      merge[cluster_id_counter, ] <- c(as.integer(ci_name), as.integer(cj_name))
      height[cluster_id_counter] <- min_dist

      cluster_i_size=length(clusters[[ci_name]])
      cluster_j_size=length(clusters[[cj_name]])

      clusters_to_remove <- sort(pair, decreasing = TRUE)
      clusters[[clusters_to_remove[1]]] <- NULL
      clusters[[clusters_to_remove[2]]] <- NULL


      not_fused_clusters<-clusters      #h>cluster die in dem schritt nicht fusioniert wurden
      clusters[[as.character(cluster_id_counter)]]<-new_cluster_points
      #Distanzen zu nicht fusionierten Clustern neu berechnen
      #cluster die in dem schritt nicht fusioniert wurden->h
      #Cluster die fusioniert wurden i und j
      #neu gebildetes Cluster k
      new_distances<-numeric(length(not_fused_clusters))
      k<-new_cluster_name
      if (link_crit=="UPGMA"){
        alpha_i=cluster_i_size/(cluster_i_size+cluster_j_size)
        alpha_j=cluster_j_size/(cluster_i_size+cluster_j_size)

      }
      if (length(not_fused_clusters)>0){
        for (i in 1:length(not_fused_clusters)){

          h<-as.integer(not_fused_clusters[[i]])
          h_name<-names(not_fused_clusters)[i]
          d_hi<-dist[h_name,ci_name]
          d_hj<-dist[h_name,cj_name]
          d_ij<-min_dist
          d_hk<-alpha_i*d_hi+alpha_j*d_hj+beta*d_ij+gamma*abs(d_hi-d_hj)
          new_distances[i]<-d_hk
        }
        # Altes aus Distanzmatrix entfernen
        dist <- dist[!(rownames(dist) %in% pair), !(colnames(dist) %in% pair)]
        #Distanzmatrix aktualisieren
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

        cluster_id_counter <- cluster_id_counter + 1
      }
    }



  #hclust-Objekt nachahmen
  hc <- list(
    merge = merge,
    height = height,
    order = getOrder_dfs(merge),
    labels = "labels",
    method = link_crit,
    call = match.call(),
    dist.method = "dist_crit"
  )

  class(hc) <- "hclust"
  return(hc)
}

cluster_both<-function(dist_pat,dist_gene,alpha_i,alpha_j,beta,gamma,link_crit=""){
  pat_clustering=agglomerative_clustering_base_algo(dist=dist_pat,alpha_i=alpha_i,alpha_j=alpha_j,beta=beta,gamma=gamma,link_crit=link_crit)
  gene_clustering=agglomerative_clustering_base_algo(dist=dist_gene,alpha_i=alpha_i,alpha_j=alpha_j,beta=beta,gamma=gamma,link_crit=link_crit)
  return(list(
    pat_clustering = pat_clustering,
    gene_clustering = gene_clustering
  ))

}


calc_params_flexible<-function(beta){
  alphas<-1-beta
  alpha_i=alpha_j=alphas/2
  gamma=0
  return(c(alpha_i = alpha_i, alpha_j = alpha_j, beta = beta, gamma = gamma))
}

