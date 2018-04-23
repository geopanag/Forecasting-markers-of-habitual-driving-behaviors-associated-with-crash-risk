#---------------------------------------------------
# Run classification experiments with xgboost
#   1. Set XGBoost parameters to the result of the hyerparameter optimization
#   2. Run 5 fold cv
#   3. Plot the AUC and store the results
#---------------------------------------------------

libs = c("xgboost","caret","dplyr","ggplot2","pROC")
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

dat = read.csv(paste0("data_3.csv"))

for(act in c("distraction","aggressiveness","arousal")){
  
  if(act=="arousal"){
    ddat= dat[complete.cases(dat),]
  }else{
    ddat = dat  
  }
  
  xgb_m = xgb.cv(   params               = param,
                    data = as.matrix(ddat %>% select(-aggressiveness,-distraction,-arousal,-drive,-subject)) ,
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

  #------------- Keep AUC curves
  ggplot(data =xgb_m$evaluation_log,aes(x=iter))+geom_line(aes(y = train_auc_mean,colour="Train") )+
    geom_line(aes(y = test_auc_mean,colour="Test"))+ylab("")+xlab("")+ggtitle(act)
  
  ggsave(paste0("../Figures/cv_auc_",act,".png"))
  
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

write.csv(dat,"data_4.csv",row.names=F)

dat$distraction_act = ifelse(dat$distraction_prediction>=0.5,1,0)
dat$aggressiveness_act = ifelse(dat$aggressiveness_prediction>=0.5,1,0)
dat$arousal_act = ifelse(dat$arousal_prediction>=0.5,1,0)
write.csv(dat,"data_5.csv",row.names=F)

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

               
results = data.frame(Distraction = compute_results(dat %>% select(distraction_act,distraction)),
                     Aggressiveness =  compute_results(dat %>% select(aggressiveness_act,aggressiveness)),
                     Arousal = compute_results(dat %>% select(arousal_act,arousal)))


results = rbind(aucs,results)
results = results*100

results = sapply(results, function(x) as.numeric(format(round(x,3),nsmall=3)))
row.names(results) = c("Area Under Curve","Accuracy","Specificity","Sensitivty","Positive Predictive Value","Negative Predictive Value")

write.csv(results,"Results/evaluation_metrics.csv")

#---------------------  Run experiments with other algorithms for distraction and aggressiveness
dat$distraction_base = 0

base_dist_acc = sum(dat$distraction_base== dat$distraction)/nrow(dat)
pred = prediction(dat$distraction_base,dat$distraction)
bas_dist_auc = format(round(unlist(slot( performance(pred,"auc"),"y.values")),3),nsmall=3)

train_control = trainControl(method="cv", number=5, savePredictions = TRUE)
glm_m = caret::train(factor(distraction)~., data=dat[,1:55], trControl=train_control, method="glmnet")
glm_dist_acc = mean(data.frame(glm_m$pred %>% group_by(Resample) %>% dplyr::summarise(acc = sum(pred==obs)/n()))[,2])
glm_dist_auc = as.numeric(roc(as.numeric(glm_m$pred$pred),as.numeric(glm_m$pred$obs) )$auc)

nb_m = caret::train(factor(distraction)~., data=dat[,1:55], trControl=train_control, method="nb")
nb_dist_acc = mean(data.frame(nb_m$pred %>% group_by(Resample) %>% dplyr::summarise(acc = sum(pred==obs)/n()))[,2])
nb_dist_auc = as.numeric(roc(as.numeric(nb_m$pred$pred),as.numeric(nb_m$pred$obs) )$auc)

svm_m = caret::train(factor(distraction)~., data=dat[,1:55], trControl=train_control, method="svmRadial")
svm_dist_acc = mean(data.frame(svm_m$pred %>% group_by(Resample) %>% dplyr::summarise(acc = sum(pred==obs)/n()))[,2])
svm_dist_auc = as.numeric(roc(as.numeric(svm_m$pred$pred),as.numeric(svm_m$pred$obs) )$auc)


comp =  rbind(c(base_dist_acc,bas_dist_auc),
c(nb_dist_acc,nb_dist_auc),
c(glm_dist_acc,glm_dist_auc),
c(svm_dist_acc,svm_dist_auc))

write.csv(comp,"Results/compare_distraction.csv")

#------------------------------------------------------------------
dat$aggressiveness_base = 0

base_dist_acc = sum(dat$aggressiveness_base== dat$aggressiveness)/nrow(dat)
pred = prediction(dat$aggressiveness_base,dat$aggressiveness)
bas_dist_auc = format(round(unlist(slot( performance(pred,"auc"),"y.values")),3),nsmall=3)

train_control = trainControl(method="cv", number=5, savePredictions = TRUE)
glm_m = caret::train(factor(aggressiveness)~., data=dat[,c(1:54,56)], trControl=train_control, method="glmnet")
glm_dist_acc = mean(data.frame(glm_m$pred %>% group_by(Resample) %>% dplyr::summarise(acc = sum(pred==obs)/n()))[,2])
glm_dist_auc = as.numeric(roc(as.numeric(glm_m$pred$pred),as.numeric(glm_m$pred$obs) )$auc)

nb_m = caret::train(factor(aggressiveness)~., data=dat[,c(1:54,56)], trControl=train_control, method="nb")
nb_dist_acc = mean(data.frame(nb_m$pred %>% group_by(Resample) %>% dplyr::summarise(acc = sum(pred==obs)/n()))[,2])
nb_dist_auc = as.numeric(roc(as.numeric(nb_m$pred$pred),as.numeric(nb_m$pred$obs) )$auc)


svm_m = caret::train(factor(aggressiveness)~., data=dat[,c(1:54,56)], trControl=train_control, method="svmRadial")
svm_dist_acc = mean(data.frame(svm_m$pred %>% group_by(Resample) %>% dplyr::summarise(acc = sum(pred==obs)/n()))[,2])
svm_dist_auc = as.numeric(roc(as.numeric(svm_m$pred$pred),as.numeric(svm_m$pred$obs) )$auc)


comp =  rbind(c(base_dist_acc,bas_dist_auc),
              c(nb_dist_acc,nb_dist_auc),
              c(glm_dist_acc,glm_dist_auc),
              c(svm_dist_acc,svm_dist_auc))

write.csv(comp,"Results/compare_aggressiveness.csv")

#------------------------------------------------------------------------------------------------

xgb_m = xgboost( params               = param,
                 data = as.matrix(dat[,1:54]),
                 label = dat$aggressiveness,
                 nrounds             = cv.nround,
                 verbose             = F,
                 maximize            = T,
                 metrics  = "auc")

importance_matrix1  = xgb.importance(colnames(dat), model = xgb_m)

to_plot = importance_matrix1[c(1:6,50:54),1:2]
to_plot$Feature[6] = ""
to_plot$Feature = factor(to_plot$Feature, levels = to_plot$Feature)
to_plot$Gain[6]=0

ggplot(dat=to_plot,aes(x=Feature,y=Gain))+geom_bar(stat="identity")+
  theme(axis.text=element_text(angle=90,size=13), axis.ticks.x = element_blank(),
        axis.title=element_text(size=14,face="bold"))
  
ggsave("../Figures/xgb_importances_aggressiveness.png")

xgb_m = xgboost( params               = param,
                 data = as.matrix(dat[,1:54]),
                 label = dat$distraction,
                 nrounds             = cv.nround,
                 verbose             = F,
                 maximize            = T,
                 metrics  = "auc")

importance_matrix2  = xgb.importance(colnames(dat), model = xgb_m)
to_plot = importance_matrix2[c(1:6,50:54),1:2]
to_plot$Feature[6] = ""
to_plot$Feature = factor(to_plot$Feature, levels = to_plot$Feature)
to_plot$Gain[6]=0

ggplot(dat=to_plot,aes(x=Feature,y=Gain))+geom_bar(stat="identity")+
  theme(axis.text=element_text(angle=90,size=13), axis.ticks.x = element_blank(),
        axis.title=element_text(size=14,face="bold"))

ggsave("../Figures/xgb_importances_distraction.png")


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
print("----------------------------------------------------------------------------DONE classification")