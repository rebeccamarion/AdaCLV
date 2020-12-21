CalcCVIsForAll <- function(Xt, hp.vals, clust.mat, methods = c("Sil", "DB", "DBstar", "Gap", "Hart"), hp.name = "K.init"){
  
  index.mat <- cbind(hp = hp.vals)
  colnames(index.mat) <- hp.name
  
  if ("DB" %in% methods){
    DB <- apply(clust.mat, 2, function(Cs) index.DB(Xt, Cs, p=2,q=1)$DB)
    index.mat <- cbind(index.mat, DB)
  }
  
  if ("DBstar" %in% methods){
    DBstar <- apply(clust.mat, 2, function(Cs) index.DBstar(Xt, Cs))
    index.mat <- cbind(index.mat, DBstar)
  }
  
  if ("Sil" %in% methods){
    dist.Xt <- dist(Xt)
    Sil <- apply(clust.mat, 2, function(Cs) index.S(dist.Xt, Cs))
    index.mat <- cbind(index.mat, Sil)
  }
  
  if ("Gap" %in% methods){
    if (ncol(clust.mat) == 1){
      stop("Gap statistic requires more than one hp value")
    } else {
      Gap.stats <- do.call(rbind, lapply(1:(ncol(clust.mat) - 1), function(k) 
        index.Gap(Xt, cbind(clust.mat[, k], clust.mat[, k + 1]), method = "centroid")))
      Gap.stats <- rbind(Gap.stats,
                         index.Gap(Xt, cbind(clust.mat[, ncol(clust.mat)], clust.mat[, ncol(clust.mat)]), method = "centroid"))
      index.mat <- cbind(index.mat, Gap.stats)
      }
    
  }
  
  if ("Hart" %in% methods){
    Hart <- do.call(rbind, lapply(1:(ncol(clust.mat) - 1), function(k) 
      index.H(Xt, cbind(clust.mat[, k], clust.mat[, k + 1]))))
    Hart <- rbind(Hart, NA)
    index.mat <- cbind(index.mat, Hart)
  }
  
  return(index.mat)
  
}