#-----------------------------------------------
#  Run all the scripts to reproduce the analysis 
#-----------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

run_seq = c("_1_creat_simulation_dataset.R",
            "_2_feature_extraction.R",
            "_3_feature_selection.R",
            "_4_classification.R",
            "_5_plot_results.R")

sapply(run_seq,source)