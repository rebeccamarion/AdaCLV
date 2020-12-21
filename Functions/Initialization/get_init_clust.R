GetInitClust <- function(X, CLV.mode = "local", K.vals, scale.vars = F, init.type = "hclust"){

  if (init.type == "hclust"){
    
    require(ClustVarLV)
    res.init <- ClustVarLV::CLV(X = X, Xu = NULL, Xr = NULL, method = CLV.mode, 
                                sX = scale.vars, sXr = FALSE, sXu = FALSE, nmax = max(K.vals), maxiter = 1)
    init.clust <- sapply(K.vals, function(x) res.init[[x]][[1]][1, ])
  } 
  
  if (init.type == "rand"){
    
    p <- ncol(X)
    init.clust <- sapply(K.vals, function(k) GenRandomClusters(K = k, p = p))
    
  }
  
  params <- cbind.data.frame(scaled = scale.vars, init = init.type, 
                             mode = ifelse(init.type == "hclust", CLV.mode, NA))
  
  return(list(init.clust = init.clust, params = params))
}