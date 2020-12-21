AdaCLVGetThresholds <- function(Z, A, eta = 3, iterative = T){
  covar <- t(Z)%*%(A)/(nrow(Z) - 1)
  thresholds <- apply(covar, 2, function(z) GetIterativeThresh(z, eta = eta))
  
  return(thresholds)
}