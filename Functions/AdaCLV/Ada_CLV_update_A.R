AdaCLVUpdateA <- function(Z, S){
  
    A <- Z%*%S 
    A.scaled <- ScaleNonzeroVar(A, center = T)
    return(A.scaled)
    
}