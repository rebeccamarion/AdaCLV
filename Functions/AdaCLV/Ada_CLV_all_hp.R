AdaCLV.AllHP <- function(X, scale.vars, prop.vals.eta.max, init.clust){
  
  K.vals <- apply(init.clust$init.clust, 2, n.unique)
  all.hp <- expand.grid(prop.eta.max = prop.vals.eta.max,
                        K.init = K.vals)
  
  res.AdaCLV <- apply(all.hp, 1, function(x) 
    AdaCLVAlgo(X = X,                                      
               scale.vars = scale.vars,
               prop.eta.max = x[1],                                     
               clusters = init.clust$init.clust[, which(K.vals == x[2])]))
  res <- list(output = res.AdaCLV,
              hp = all.hp)
  return(res)
}