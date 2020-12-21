AdaCLVUpdateS <- function(Z, A, tau){
  
  
  covar <- t(Z)%*%(A)*(1/(nrow(Z) - 1))
  S.init <- pmax(covar - rep(1, nrow(covar))%*%t(tau), 0)
  S.scaled <- ScaleNonzeroL2Norm(S.init, center = F)
  
  return(S.scaled)
  
}