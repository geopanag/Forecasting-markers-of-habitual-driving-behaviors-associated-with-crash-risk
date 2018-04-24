#-----------------------------------------------
#  Run all the scripts to reproduce the analysis 
#-----------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

run_seq = c("_1_creat_simulation_dataset.R",
            "_2_feature_extraction.R",
            "_3_feature_selection.R",
            "_4_classification.R",
            "_5_plot_results.R")


source("_1_create_simulation_dataset.R")
source("_2_feature_extraction.R")
sapply(run_seq,source)

