BestMaxDec <- function(x, hp.vals){
  # max_k i_{k-1} - i_{k}
  diffs <- x[1:(length(x)-1)] - x[2:length(x)]
  wh <- which.max(diffs) + 1
  return(hp.vals[wh])
}
