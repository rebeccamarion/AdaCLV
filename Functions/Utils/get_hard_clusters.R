GetHardClusters <- function(S){
  
  clusters <- apply(S, 1, which.max)
  wh.zero <- which(rowSums(abs(S)) == 0)
  clusters[wh.zero] <- max(clusters) + 1
  clusters <- as.integer(factor(paste(clusters)))
  
  return(clusters)
}