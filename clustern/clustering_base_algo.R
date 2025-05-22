agglomerative_clustering_base_algo<-function(data=NULL,dist,distance_measure,linkage_criterium,cluster_rows,cluster_cols){
  if (linkage_criterium=="single linkage"){
    alpha_i=0.5
    alpha_j=0.5
    beta=0
    gamma=-0.5
  }
  if (cluster_cols){
    #dist=  dist_mat <- as.matrix(dist((t(data)), method = distance_measure))
    #dist<-c(0,1,2,9,13,1,0,5,10,10,2,5,0,5,13,9,10,5,0,4,13,10,13,4,0)
    #dim(dist)<-c(5,5)
    n<-ncol(data)
    colnames(dist)<-as.character(-(1:n))
    rownames(dist)<-as.character(-(1:n))
    clusters <- lapply(1:n, function(i) -i)
    names(clusters)<-as.character(-(1:n))
    labels<-colnames(data)
    
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
      
      
      idx_ci <- if (length(ci_points) == 1) ci_points else ci_name  # Wenn Einzelpunkt, dann negativ
      idx_cj <- if (length(cj_points) == 1) cj_points else cj_name
      
      merge[cluster_id_counter, ] <- c(as.integer(idx_ci), as.integer(idx_cj))
      height[cluster_id_counter] <- min_dist
      
      
      
      idx_to_remove <- sort(pair, decreasing = TRUE)
      
      clusters[[idx_to_remove[1]]] <- NULL
      clusters[[idx_to_remove[2]]] <- NULL
      #cluster die in dem schritt nicht fusioniert wurden->h
      not_fused_clusters<-clusters[!(names(clusters) %in% c(ci_name, cj_name))]
      clusters[[as.character(cluster_id_counter)]]<-new_cluster_points#MÜSSTE GLEICH SEIN
      #Distanzen zu nicht fusionierten Clustern neu berechnen
      #cluster die in dem schritt nicht fusioniert wurden->h
      #Cluster die fusioniert wurden i und j
      #neu gebildetes Cluster k
      new_distances<-numeric(length(not_fused_clusters))
      k<-new_cluster_name
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
        new_dists<-matrix(0,nrow=length(clusters),ncol=length(clusters))
        colnames(new_dists)<-names(clusters)
        rownames(new_dists)<-names(clusters)
        new_dists[nrow(new_dists),1:(ncol(new_dists)-1)]<-new_distances
        new_dists[1:(nrow(new_dists)-1),ncol(new_dists)]<-new_distances
        if (is.matrix(dist) && nrow(dist) > 0 && ncol(dist) > 0){
          new_dists[rownames(dist),colnames(dist)]<-dist[rownames(dist),colnames(dist)]
          
        }
        
        
        dist<- new_dists
        
        cluster_id_counter <- cluster_id_counter + 1
      }
    }
    
    
  }
  #hclust-Objekt nachahmen
  hc <- list(
    merge = merge,
    height = height,
    order = 1:n,
    labels = labels,
    method = linkage_criterium,
    call = match.call(),
    dist.method = distance_measure
  )
  
  class(hc) <- "hclust"
  return(hc)
}



load("TCGA_kidney_unnormalized.RData")
data=dataset$data
small_data=data[1:5,1:5]
dist=  dist_mat <- as.matrix(dist((t(small_data)), method = "euclidean"))
result_small<-agglomerative_clustering_base_algo(data=small_data,dist=dist,distance_measure="euclidean","single linkage",FALSE,TRUE)
merge=result_small$merge
dist<-c(0,1,2,9,13,1,0,5,10,10,2,5,0,5,13,9,10,5,0,4,13,10,13,4,0)
dim(dist)<-c(5,5)
result_test<-agglomerative_clustering_base_algo(data=small_data,dist=dist,"euclidean","single linkage",FALSE,TRUE)
merge_test<-result_test$merge
hcl_test<-hclust(as.dist(dist), method="single",members=NULL)
merge_hclust<-hcl_test$merge
hcl_small<-hclust(dist(t(small_data)), method="single",members=NULL)