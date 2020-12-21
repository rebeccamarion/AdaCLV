GetBestHP1ForSingleHP2 <- function(Xt, 
                                   res.AdaCLV, 
                                   res.indexes,
                                   hp1.vals,
                                   CVI.methods = c("Sil", "DB", "DBstar", "Gap", "Hart"),
                                   decision.rules = c("Max", "Min", "Min", "MaxInc", "MaxDec"),
                                   hp1 = "K.init",
                                   hp2 = "prop.eta.max",
                                   hp2.val = NULL){
  
  clust.mat <- sapply(res.indexes, function(x) GetHardClusters(res.AdaCLV$output[[x]]$S))
  hp1.vals <- sort(res.AdaCLV$hp[res.indexes, hp1])
  
  index.mat <- CalcCVIsForAll(Xt, hp1.vals, clust.mat, CVI.methods, hp.name = hp1)
  
  decision.mat <- do.call(rbind, lapply(1:length(CVI.methods), function(CVI.index) 
    GetBestHPSingleIndex(index.mat, 
                         CVI.name = CVI.methods[CVI.index],
                         decision.rule = decision.rules[CVI.index],
                         hp.name = hp1)))
  
  decision.mat <- cbind.data.frame(rbind(setNames(hp2.val, hp2)), decision.mat)
  
  
  return(decision.mat)
  
}