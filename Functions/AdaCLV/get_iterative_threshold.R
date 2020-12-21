GetIterativeThresh <- function(x, eta = 3){
  
  converged <- F
  signal.list <- list()
  indexes <- 1:length(x)
  iter <- 1
  indexes.sub <- indexes
  x.temp <- x
  
  while (converged == F){
    
    mean.x <- mean(x.temp)
    sd.x <- sd(x.temp)
    
    signal.list[[iter]] <- indexes.sub[which(abs(x.temp) > (mean.x + (eta*sd.x)))]
    
    
    if (length(signal.list[[iter]]) == 0){
      converged <- T
    } else {
      all.signal <- sort(unique(unlist(signal.list)))
      x.temp <- x[-all.signal]
      indexes.sub <- indexes[-all.signal]
      iter <- iter + 1
      
    }
    
    
  }
  
  all.signal <- sort(unique(unlist(signal.list)))
  if (length(all.signal) > 0){
    # No noise variables --> lowest threshold possible = 0
    if (length(all.signal) == length(x)){
      thresh <- 0
      x.noise <- NULL
    } else {
      thresh <- max(x[-all.signal])
      x.noise <- x[-all.signal]
    }
    
  } else {
    thresh <- max(x)
    x.noise <- x
  }
  
  thresh <- pmax(thresh, 0)
  
  return(list(thresh = thresh, noise.covar = x.noise))
}
