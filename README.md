# AdaCLV

This is an implementation of Adaptive Clustering around Latent Variables (AdaCLV) in R. AdaCLV is a method for simultaneous dimensionality reduction and variable clustering (see Marion, R., Govaerts, B., & von Sachs, R. (2020). AdaCLV for interpretable variable clustering and dimensionality reduction of spectroscopic data. Chemometrics and Intelligent Laboratory Systems.).

This code was written by Rebecca Marion (Université catholique de Louvain, https://sites.google.com/view/rebeccamarion/).

Dependencies: ClustVarLV package 

## Running AdaCLV

The file "Run_AdaCLV.R" in the Scripts folder is a sample script for running AdaCLV and calibrating its hyperparameters. To run this script:

1. Add your data file to the folder "Data." This data file must be an .RData file containing a list object named "data" containing a matrix named "X" of observed data (observations x variables).
2. Update the REQUIRED USER INPUT section of "Run_AdaCLV.R"
  * Define data.file.name using the name of your .RData file (e.g. data.file.name <- "Citrate_Hippurate.RData")
  * Define data.name using a desired identifier for the output files (e.g. data.name <- "Citrate_Hippurate")
3. (Optional) Update the OPTIONAL USER INPUT section of "Run_AdaCLV.R" if you wish to change the default settings.
4. Run the file "Run_AdaCLV.R"

## Output files and plots

Running the file "Run_AdaCLV.R" will generate results and save them to the "Data" folder:
1. Results for all hyperparameters (a list object named "res.AdaCLV"): "AdaCLV_results_all_hp_[data.name].RData"
  * res.AdaCLV$output: list of results, each element corresponds to a hyperparameter setting
  * res.AdaCLV$hp: dataframe of hyperparameter settings, each row corresponds to an element in the list res.AdaCLV$output
2. Summary of best hyperparameter settings according to each Cluster Validity Index (CVI)/Decision Rule pair (a dataframe object named "best.hp.AdaCLV"): "AdaCLV_best_hp_settings_[data.name].RData"
3. Results for the best setting according to [CVI.method] and [Decision.rule] (a list object named "best.res.AdaCLV"): "AdaCLV_best_results_[CVI.methodDecision.rule]_[data.name].RData"


In addition to these output files, the script "Run_AdaCLV.R" generates plots of cluster membership degrees for the best hyperparameter settings (one plot for each Cluster Validity Index).  

# References

* Marion, R., Govaerts, B., & von Sachs, R. (2020). AdaCLV for interpretable variable clustering and dimensionality reduction of spectroscopic data. Chemometrics and Intelligent Laboratory Systems.

* Kaczynska, S., Marion, R., & von Sachs, R. (2020). Comparison of cluster validity indices and decision rules for different degrees of cluster separation, In Proceedings of the European Symposium on Artificial Neural Networks, Computational Intelligence and Machine Learning (ESANN). 
