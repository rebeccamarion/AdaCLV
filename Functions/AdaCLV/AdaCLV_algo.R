AdaCLVAlgo <- function(X, 
                       scale.vars = F,
                       eps = 1e-5, 
                       max.iter = 300,
                       prop.eta.max = 0.5,
                       clusters = NULL){

# Prepare inputs/outputs

Z <- NormalizeX(X, scale.vars)

S.list <- list()
A.list <- list()
tau.list <- list()

## Prepare initialization

clusters <- as.integer(factor(paste(clusters)))
S.init <- MakeSMatrix(clusters)
S <- ScaleNonzeroL2Norm(S.init, center = F)

## First iteration

# Update A
A <- AdaCLVUpdateA(Z = Z, 
                   S = S)
A.list[[1]] <- A

# Calculate maximum eta value and fix eta as a proportion of the max
eta.max <- GetMaximumEtaVal(Z, A)
eta <- prop.eta.max*eta.max

# Update thresholds
thresh.res <- AdaCLVGetThresholds(Z, A, eta = eta)
tau.list[[1]] <- sapply(thresh.res, function(x) x$thresh)

# Update S
S <- AdaCLVUpdateS(Z = Z, 
                   A = A, 
                   tau = tau.list[[1]])
S.list[[1]] <- S

crit.init <- AdaCLVObjective(Z, A, S, tau = tau.list[[1]])
crit <- list(crit.init)
crit.diff <- Inf

i <- 1

while (i <= max.iter 
       && crit.diff > eps
){
  
  # Update A
  A <- AdaCLVUpdateA(Z = Z, 
                     S = S.list[[i]])
  A.list[[i + 1]] <- A
  
  
  # Update thresholds
  thresh.res <- AdaCLVGetThresholds(Z, A, eta = eta)
  tau.list[[i + 1]] <- sapply(thresh.res, function(x) x$thresh)
  
  # Update S
  S <- AdaCLVUpdateS(Z = Z, 
                     A = A, 
                     tau = tau.list[[i + 1]])
  S.list[[i + 1]] <- S
  
  # Calculate criterion
  crit[[i + 1]] <- AdaCLVObjective(Z, A, S, tau = tau.list[[i + 1]])
  crit.diff <- abs(crit[[i + 1]] - crit[[i]])
  i <- i + 1
  
}


return(list(A = A.list[[i]],
            S = S.list[[i]],
            crit.final = crit[[i]],
            crit.hist = crit,
            tau = tau.list[[i-1]],
            eta.max = eta.max,
            converged = i <= max.iter))
}
