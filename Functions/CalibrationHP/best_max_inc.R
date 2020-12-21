BestMaxInc <- function(x, hp.vals){
  # max_k i_{k} - i_{k-1}
  diffs <- x[2:(length(x))] - x[1:(length(x) - 1)]
  wh <- which.max(diffs) + 1
  return(hp.vals[wh])
}