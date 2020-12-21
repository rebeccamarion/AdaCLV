MakeSMatrix <- function(clusters){
  
    clust.vals <- sort(unique(clusters))
    wh.nonzero <- which(clust.vals != 0)
    if (length(wh.nonzero) == 0){
      S <- matrix(0, nrow = length(clusters), ncol = 1)
    } else {
      
      S <- sapply(clust.vals[wh.nonzero], function(y) {
        s_m <- rep(0, length(clusters))
        s_m[which(clusters == y)] <- 1
        return(s_m)
      })
      
    }
    
    
  return(S)
  
}