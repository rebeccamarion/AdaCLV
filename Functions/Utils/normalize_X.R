NormalizeX <- function(X, scale.vars = F){
  
  X.norm <- scale(X, center = T, scale = scale.vars)
  
  return(X.norm)
}