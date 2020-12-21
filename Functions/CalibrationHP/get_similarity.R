#Function that calculates the similarity measure as defined in (Davies & Bouldin, 1979)
#Inputs: full: matrix of data points where the last column corresponds to the assignment of the data point to a cluster, 
#centroids: matrix of centroids, k: the number of clusters
#Output: vector of similarity measures of length k

get.similarity <- function(full, centroids, k){
  distances <-list()
  for(i in 1:k){ #go over all clusters and calculate distances between points and centroid of this cluster
    temp = full[full[,ncol(full)]==i,,drop = FALSE] #points beloning to cluster i
    if(nrow(temp) ==1){
      distances[[i]]<-sapply(temp[,-ncol(temp)], FUN = function(x) dist(rbind(x, centroids[i,])))
    }else{
      distances[[i]] <- apply(X = temp[,-ncol(temp)], 1, FUN = function(x) dist(rbind(x, centroids[i,]))) 
      #distances from each point in this cluster to its centroid
    }
  }
  similarities <- sapply(distances, FUN = mean) #similarities for each c_k as average of distances
  return(similarities)
}