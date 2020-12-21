AdaCLVObjective <- function(Z, A, S, tau){
  mean(sapply(1:ncol(S), function(k) t(S[, k])%*%(t(Z)%*%A[, k] - tau[k])))
}