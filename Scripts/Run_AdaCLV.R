##########################################
#### Sample script for running AdaCLV ####
##########################################

# Code written by Rebecca Marion, Université catholique de Louvain
# Publication reference: 
  # Marion, R., Govaerts, B., & von Sachs, R. (2020). 
  # AdaCLV for interpretable variable clustering and dimensionality reduction of spectroscopic data. 
  # Chemometrics and Intelligent Laboratory Systems.

rm(list = ls())

library(ClustVarLV)

#### REQUIRED USER INPUT ####

## Required input from user:
 # data.file.name: name of RData file 
    # RData file contains a list named "data" containing a matrix named "X" (observations x variables)
 # data.name: data name to be used in results file names

data.file.name <- "citrate_hippurate_processed.RData"
data.name <- "citrate_hippurate"

#### OPTIONAL USER INPUT ####

## Other parameters that can be changed:
 # K.vals: vector of integers representing the number of initial clusters 
 # prop.vals.eta.max: vector of proportions in the interval [0, 1] defining the sparsity hyperparameter eta
    # eta is set to a proportion of the maximum eta value (this maximum is calculated automatically)
 # scale.vars: TRUE if variables should be scaled, FALSE otherwise
 # init.type: type of initialization ("rand" = random, "hclust" = Vigneau et al. (2003) hierarchical algorithm)
 # CVI.methods: vector with names of cluster validity indices to be used for calibration 
      # See Kaczynska, S., Marion, R., & von Sachs, R. (2020). Comparison of cluster validity indices and
        # decision rules for different degrees of cluster separation.
      # Options: "Sil", "DB", "DBstar", "Gap", "Hart"
 # decision.rules: vector with names of decision rules to be used for calibration 
      # See Kaczynska, S., Marion, R., & von Sachs, R. (2020). Comparison of cluster validity indices and
        # decision rules for different degrees of cluster separation.
      # Options: "Max", "Min", "MaxInc", "MaxDec", "Gap"
        # Decision rules for DB, DBstar and Hart: "MaxDec" and "Min"
        # Decision rules for Sil: "MaxInc" and "Max"
        # Decision rules for Gap: "MaxInc" and "Gap"
      # Note: Element i of decision.rules is the decision rule for CVI i
        

K.vals <- 2:8
prop.vals.eta.max <- seq(0.05, 0.95, by = 0.05)
scale.vars <- F
init.type <- "hclust"
CVI.methods <- c("Sil", "Gap", "Hart")
decision.rules <- c("MaxInc", "MaxInc", "MaxDec")

#### TO RUN ####

## Load functions
function.path <- "../Functions/"
source("../Scripts/Load_all_functions.R")

## Load data
load(paste0("../Data/", data.file.name))
X <- ConstantSumNormalization(data$X)

## Get initial clusters
if (init.type == "rand"){
  set.seed(8298)
}
init.clust <- GetInitClust(X = X,
                           K.vals = K.vals,
                           scale.vars = scale.vars,
                           init.type = init.type)

## Run AdaCLV
res.AdaCLV <- AdaCLV.AllHP(X = X, 
                           scale.vars = scale.vars,
                           prop.vals.eta.max = prop.vals.eta.max,
                           init.clust = init.clust)


## Choose best hyperparameters

# Step 1: Get best K.init value for each candidate eta value --> Hyperparameter candidates (K.init, eta) for each eta

# Get best K.init vals for fixed eta
decisions.by.eta <- GetBestHP1ByHP2(Xt = t(X), 
                                  res.AdaCLV, 
                                  CVI.methods,
                                  decision.rules,
                                  hp1 = "K.init",
                                  hp2 = "prop.eta.max")

# Step 2: Choose the best hyperparameter pair (candidate pair with best eta)

final.decisions <- NULL
for (CVI.index in 1:length(CVI.methods)){
  
  method.decisions <- decisions.by.eta[which(decisions.by.eta$CVI.name == CVI.methods[CVI.index]), , drop = F]
  index.mat <- cbind.data.frame(method.decisions, 
                                setNames(as.data.frame(method.decisions$index.val), 
                                         CVI.methods[CVI.index]))
  
  
  best.prop.eta.max <- GetBestHPSingleIndex(index.mat = index.mat, 
                                            CVI.name = CVI.methods[CVI.index],
                                            decision.rule = decision.rules[CVI.index],
                                            hp.name = "prop.eta.max")$prop.eta.max
  
  final.decisions <- rbind.data.frame(final.decisions,
                                      method.decisions[which(method.decisions$prop.eta.max == best.prop.eta.max), ])
  
}
best.hp.AdaCLV <- final.decisions

## Plot cluster memberships for best hp choices

for (CVI.index in 1:nrow(final.decisions)){
  
  hp.index <- final.decisions$index.best.hp[CVI.index]
  prop.eta.max.val <- res.AdaCLV$hp[hp.index, "prop.eta.max"]
  eta.val <- round(prop.eta.max.val*res.AdaCLV$output[[hp.index]]$eta.max, 2)
  K.init <- res.AdaCLV$hp[hp.index, "K.init"]
  S <- res.AdaCLV$output[[hp.index]]$S
  
  plot.title <- bquote(eta == .(prop.eta.max.val) ~ {eta[max]  ==  .(eta.val)} ~ ", " ~ K[init] == .(K.init))
  plot.subtitle <- paste0("Calibration Method: ", CVI.methods[CVI.index], " with ", decision.rules[CVI.index], " rule")
  
  PlotS(S, main = plot.title, ylab = "Membership Degree", yaxt = "s")
  mtext(plot.subtitle)
  
}

## Save results

# All hp
save(res.AdaCLV, 
     file = paste0("../Data/AdaCLV_results_all_hp_", data.name, ".RData"))

# Matrix of best hp settings according to each CVI method used
save(best.hp.AdaCLV, 
     file = paste0("../Data/AdaCLV_best_hp_settings_", data.name, ".RData"))

# Best AdaCLV results according to each CVI method
for (CVI.index in 1:nrow(final.decisions)){
  best.res.AdaCLV <- res.AdaCLV$output[[best.hp.AdaCLV$index.best.hp[CVI.index]]]
  save(best.res.AdaCLV, 
       file = paste0("../Data/AdaCLV_best_results_", CVI.methods[CVI.index], decision.rules[CVI.index], "_", data.name, ".RData"))
}
