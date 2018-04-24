#-----------------------------------------------
#  Run all the scripts to reproduce the analysis 
#-----------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

run_seq = c("_1_create_simulation_dataset.R",
            "_2_feature_extraction.R",
            "_3_feature_selection.R",
            "_4_classification.R",
            "_5_plot_results.R")

start_time <- Sys.time()
sapply(run_seq,source)
x = Sys.time() - start_time
write.csv(x,"../time.csv")

