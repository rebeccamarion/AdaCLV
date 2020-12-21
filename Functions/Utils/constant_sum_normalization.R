ConstantSumNormalization <- function(X){
  
  row.sums <- rowSums(X)
  X.new <- t(scale(t(X), center = F, scale = row.sums))
 
  return(X.new)
}