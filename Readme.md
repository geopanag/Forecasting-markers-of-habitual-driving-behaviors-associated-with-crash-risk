
Code to reproduce the analysis of the paper "Forecasting markers of habitual driving behaviors associated with crash risk"  <br />

The folder structure of the project to reproduce the analysis should be like this:  <br />


Code-> ( Fill it with this code ) <br />
Data-> Raw ( fill it with the data found under Raw Thermal Data folder in https://osf.io/c42cn/) <br />
Data-> Dataset-Table-Index.csv
Data-> Results (will be filled by the scripts)
Figures (will be filled by the scripts)

Run Instructions<br />

Install R libraries: 
"signal","dplyr","xlsx","TTR",
"pastecs","ggplot2","caret","zoo","tibble","lomb"
"reshape2","glmnet","xgboost","ROCR","pROC"
<br />


Run Run.R to reproduce the analysis.
Each script can be run individually, following the order of the number in the script's title. 
If two scripts have the same number, they can be run simultaneously. 
Data visualization  app can be accessed here https://georgepanagopoulos.shinyapps.io/StressAndAggressivenessOnRoadPrediction/
