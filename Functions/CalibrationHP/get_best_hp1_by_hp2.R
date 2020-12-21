GetBestHP1ByHP2 <- function(Xt, 
                            res.AdaCLV, 
                            CVI.methods = c("Sil", "DB", "DBstar", "Gap", "Hart"),
                            decision.rules = c("Max", "Min", "Min", "MaxInc", "MaxDec"),
                            hp1 = "K.init",
                            hp2 = "prop.eta.max"){
  
  hp2.vals <- sort(unique(res.AdaCLV$hp[, hp2]))
  n.hp2 <- length(hp2.vals)
  
  
  
  decisions.by.hp2 <- list()
  for (hp2.index in 1:n.hp2){
    res.indexes <- which(res.AdaCLV$hp[, hp2] == hp2.vals[hp2.index])
    
    decisions.by.hp2[[hp2.index]] <- GetBestHP1ForSingleHP2(Xt, 
                                                            res.AdaCLV, 
                                                            res.indexes,
                                                            hp1.vals,
                                                            CVI.methods,
                                                            decision.rules,
                                                            hp1,
                                                            hp2,
                                                            hp2.val = hp2.vals[hp2.index])
    
  }
  decisions <- do.call(rbind, decisions.by.hp2)
  
  # Which element in res.AdaCLV list corresponds to best hp
  index.best.hp <- sapply(1:nrow(decisions), function(x) 
    which(res.AdaCLV$hp[, hp2] == decisions[x, hp2] &
            res.AdaCLV$hp[, hp1] == decisions[x, hp1]))
  decisions <- cbind(decisions, index.best.hp = index.best.hp)
  
  return(decisions)
}