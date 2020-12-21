#Function that calculates the DB* index as defined in (Kim & Ramakrishna 2005)
#Inputs: data where each data point is on one row, partition: vector of integers indicating to which cluster the point belongs to, k: the number of clusters
#Output: the DB* index
index.DBstar<- function(data, partition){
  full<- as.data.frame(cbind(data, partition))
  k <- n.unique(partition)
  s <- split(full[,-ncol(full)], full[,ncol(full)], drop = FALSE) #split data by cluster
  centroids <- t(sapply(s, colMeans)) #calculate means column wise: centroids of each cluster
  centroids_distances <- as.matrix(dist(centroids,upper = T, diag = F))
  similarities <- get.similarity(full,centroids,k)
  index = 0 #intitialise sum
  for(j in 1:k){
    numerator = max(similarities[similarities!=similarities[j]])+similarities[[j]]
    denominator = min(centroids_distances[j,][centroids_distances[j,]!=0])
    index = index + (numerator/denominator) #increment index
  }
  index = index/k #final step: division by the number of clusters
  return(index)
}