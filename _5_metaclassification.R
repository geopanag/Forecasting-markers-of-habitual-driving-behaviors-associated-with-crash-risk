#---------------------------------------------------
# Run classification experiments with xgboost
#   1. Set XGBoost parameters to the result of the hyerparameter optimization
#   2. Run 5 fold cv
#   3. Plot the AUC and store the results
#---------------------------------------------------

libs = c("xgboost","caret","dplyr","ggplot2","ROCR","zoo")
lapply(libs, require, character.only = TRUE)

cv.nfold = 5
cv.nround = 500
metric = "auc"

param = list(  objective           = "binary:logistic", 
               booster             = "gbtree",
               eval_metric         = metric,
               eta                 = 0.1,
               max_depth           = 10,
               gamma=0.8,
               min_child_weight = 3,
               subsample           = 1,
               colsample_bytree    = 0.5
)


#----------------------------------------------------------

setwd("../Data")
set.seed(2018)


aucs = c()

tmp = read.csv("data_3.csv")
tmp$arousal=NULL

#-- Driving
#dat = tmp[,grepl("steering*|acceleration*|distraction|aggressiveness|arousal|drive|subject",names(tmp))]

#-- Physiology
dat = tmp[,!grepl("steering*|acceleration*|cov*",names(tmp))]

#-- All
#dat=tmp


for(act in c("distraction","aggressiveness")){
  
  if(act=="arousal"){
    ddat= dat[complete.cases(dat),]
  }else{
    ddat = dat  
  }
  
  xgb_m = xgb.cv(   params               = param,
                    data = as.matrix(ddat %>% select(-aggressiveness,-distraction,-drive,-subject)) ,
                    label =  ddat[,act],
                    nrounds             = cv.nround,
                    verbose             = F,
                    prediction          = T,
                    maximize            = T,
                    nfold = cv.nfold,
                    metrics  = metric,
                    early_stopping_rounds = 100,
                    scale_pos_weight = sum(ddat[,act]==0,na.rm=T)/sum(ddat[,act],na.rm=T))
  
  #------------- Keep the best AUC
  aucs =  c(aucs,as.numeric(xgb_m$evaluation_log[xgb_m$best_iteration,"test_auc_mean"]))
  #------------ Keep for visualization
  if(act=="distraction"){
    dat$distraction_prediction = xgb_m$pred  
  }else if(act=="arousal"){
    dat$arousal_prediction = NA
    dat[complete.cases(dat$arousal),"arousal_prediction"] = xgb_m$pred  
  }else{
    dat$aggressiveness_prediction = xgb_m$pred  
  }
  
}




#------------------------ Derive metrics for the three prediction tasks
compute_results <- function(sdat){
  sdat = sdat[complete.cases(sdat),]
  acc = sum(sdat[,1] == sdat[,2])/nrow(sdat)
  conf_mat = table(sdat)
  specif = conf_mat[1,1]/sum(conf_mat[,1])
  sensiv = conf_mat[2,2]/sum(conf_mat[,2])
  preci =  conf_mat[2,2]/sum(conf_mat[2,])
  npv =    conf_mat[1,1]/sum(conf_mat[1,])
  
  return(c(acc,specif,sensiv,preci,npv))
}

#---------------------Results for distraction with the new method
labels = c()
dist = c()
for( s in  unique(dat$subject)){
  for( d in  unique(dat$drive)){
    t = dat %>% dplyr::filter(drive==d, subject==s) %>% dplyr::select(distraction,distraction_prediction)
    if(nrow(t)>0){
      
      lab = ifelse(rollapply(t$distraction, 6, mean,by=6)>=0.5,1,0)
      act = rollapply(t$distraction_prediction, 6, mean,by=6)
      
      labels = c(labels,lab)  
      dist = c(dist,act)
    }
  }
}

#----------------------- Threshold for 1 minute flag
thres = 0.4
pred = prediction(dist,labels)
dist = ifelse(dist>=thres,1,0)

sdat = data.frame(cbind(dist,labels))
dat$aggressiveness_act = ifelse(dat$aggressiveness_prediction>=0.5,1,0)


write.csv(data.frame(as.numeric(c(dist==labels,dat$aggressiveness_act == dat$aggressiveness))),"Results/driving_proportions.csv")

#--------------------- Total results
results = data.frame(Distraction = compute_results(sdat),
                     Aggressiveness =  compute_results(dat %>% select(aggressiveness_act,aggressiveness)))

results = rbind(aucs,results)
results = results*100

results = sapply(results, function(x) as.numeric(format(round(x,3),nsmall=3)))
row.names(results) = c("Area Under Curve","Accuracy","Specificity","Sensitivty","Positive Predictive Value","Negative Predictive Value")

write.csv(results,"Results/evaluation_metrics_physiology2.csv")


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
print("----------------------------------------------------------------------------DONE classification")