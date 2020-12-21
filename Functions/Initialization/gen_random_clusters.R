GenRandomClusters <- function(K, p){
  
  cand <- 1:K
  clusters <- sample(cand, size = p, replace = T)
  n.unique <- length(unique(clusters))
  while(n.unique != K){
    clusters <- sample(cand, size = p, replace = T)
    n.unique <- length(unique(clusters))
  }
  
  return(clusters)
}