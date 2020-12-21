GetMaximumEtaVal <- function(Z, A){
  R.init <- t(Z)%*%A/(nrow(Z) - 1)
  max.R <- apply(abs(R.init), 2, max)
  mean.R <- apply(R.init, 2, mean)
  sd.R <- apply(R.init, 2, sd)
  eta.max <- pmax(max((max.R - mean.R)/sd.R), 0)
  return(eta.max)
}