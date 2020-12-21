GetBestHPSingleIndex <- function(index.mat, CVI.name = "Sil", decision.rule = "MaxInc", hp.name = "K.init"){
  # Decision rules for DB, DBstar and Hart: "MaxDec" and "Min"
  # Decision rules for Sil: "MaxInc" and "Max"
  # Decision rules for Gap: "MaxInc" and "Gap"
  
  hp.vals <- index.mat[, hp.name]
  index.vals <- index.mat[, CVI.name]
  
  if (decision.rule == "MaxInc"){
    if (CVI.name %in% c("DB", "DBstar", "Hart")){
      stop(paste0("MaxInc is not recommended for ", CVI.name))
    } else {
      best.hp <- BestMaxInc(index.vals, hp.vals)
    }
  }
  
  if (decision.rule == "MaxDec"){
    if (CVI.name %in% c("Sil", "Gap")){
      stop(paste0("MaxDec is not recommended for ", CVI.name))
    } else {
      best.hp <- BestMaxDec(index.vals, hp.vals)
    }
  }
  
  if (decision.rule == "Max"){
    if (CVI.name %in% c("DB", "DBstar", "Hart")){
      stop(paste0(CVI.name, " should not be maximized"))
    } else {
      best.hp <- hp.vals[which.max(index.vals)]
    }
  }
  
  if (decision.rule == "Min"){
    if (CVI.name %in% c("Sil", "Gap")){
      stop(paste0(CVI.name, " should not be minimized"))
    } else {
        best.hp <- hp.vals[which.min(index.vals)]
      
    }
  }
  
  if (decision.rule == "Gap"){
    if (CVI.name != "Gap"){
      stop("The Gap rule can only be used with the Gap CVI")
    } else {
      best.hp <- hp.vals[which(index.mat[, "diffu"]>=0)[1]]
    }
    
  }
  
  decision <- cbind.data.frame(best.hp = best.hp, 
                               CVI.name = CVI.name, 
                               decision.rule = decision.rule, 
                               index.val = index.vals[which(hp.vals == best.hp)],
                               diffu = NA,
                               sd = NA)
  if (CVI.name == "Gap"){
    decision$diffu <- index.mat[which(hp.vals == best.hp), "diffu"]
    decision$sd <- index.mat[which(hp.vals == best.hp), "sdgap1"]
  }
  
  colnames(decision)[1] <- hp.name
  
  return(decision)
}