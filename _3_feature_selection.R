#----------------------------
# Feature selection and lag
#   1. Remove redundant features based on NA values and  correlation
#   2. Use the features selected for predicting Distraction using linear models and xgboost
#   3. Form the dataset for the forecasting task by adding lag and set labels to be one step ahead, for each subject-drive recording
#----------------------------

libs = c("reshape2","dplyr","ggplot2","glmnet","caret","xgboost","ROCR")
lapply(libs, require, character.only = TRUE)


#----------------------- Linear models
linear_feature_selection<-function(dat,lab){
  
  feature_auc = matrix(ncol=2,nrow=0)
  #feature_auc = c()
  
  for(f in names(dat)[1:which(grepl("label",names(dat)))-1]){
    auc = 0
    
    sdat_f = dat[,c(f,"label")]
    
    #---------- Stratified cross validation
    folds = createFolds(factor(sdat_f$label), k = 10)

    for(idx in folds){
      test = sdat_f[unlist(idx),]
      train = sdat_f[-unlist(idx),]

      if(sum(train$label)==0){
        next
      }

      #------- Distraction model
      fit= glm(label~., data = train, family = "binomial")
      preds = predict(fit, test, type="response")

      #------- Calculate auc
      preds = prediction(preds,test$label)
      auc_f  = as.numeric(unlist(slot(performance(preds,"auc"),"y.values")))

      auc = auc+auc_f
    }
    
    feature_auc = rbind(feature_auc,c(f,auc/10))
  }
  
  feature_auc = data.frame(feature_auc)
  names(feature_auc) = c("feature","auc")
  feature_auc$auc = as.numeric(as.character(feature_auc$auc))
  ggplot(feature_auc, aes(feature,auc))+geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle=90))
  ggsave(paste0("../Figures/lm_features",lab,".png"))
  
  return(as.character(data.frame(feature_auc %>% arrange(.,desc(auc)) %>% select(feature))[1:10,]))
  
}


#----------------------- XGB
xgb_feature_selection <-function(dat,lab) {
  cv.nround = 500
  
  param = list(  objective           = "binary:logistic", 
                 booster             = "gbtree",
                 eval_metric         = "auc",
                 eta                 = 0.1,
                 max_depth           = 10,
                 gamma=0.8,
                 min_child_weight = 3,
                 subsample           = 1,
                 colsample_bytree    = 0.5
  )
  
  xgb_m = xgboost( params               = param,
                   data = as.matrix(dat %>% select(-label)),
                   label = dat$label,
                   nrounds             = cv.nround,
                   verbose             = F,
                   maximize            = T,
                   metrics  = "auc")
  importance_matrix  = xgb.importance(colnames(dat), model = xgb_m)
  png(paste0("../Figures/xgb_features_",lab,".png"))
  xgb.plot.importance(importance_matrix[1:10,])
  dev.off()
  
  return(importance_matrix$Feature[1:10])
}


normalize <- function(x){
  (x- min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))
}


#------------------------------------------------------
setwd("../Data")

dat = read.csv("data_2.csv")
initial_columns = ncol(dat)
init = c(sum(grepl("perinasal",names(dat))),sum(grepl("breath",names(dat))),
         sum(grepl("palm",names(dat))),sum(grepl("heart",names(dat))),sum(grepl("cov|cor",names(dat))))


speed = dat$speed
acceleration = dat$acceleration
aggressiveness = dat$aggressiveness
steering = dat$steering
lane = dat$lane
drive = dat$drive

dat$speed=NULL
dat$acceleration=NULL
dat$aggressiveness = NULL
dat$steering=NULL
dat$lane=NULL

distraction = dat$distraction
dat$distraction = NULL
subjects = dat$subject
dat$subject = NULL
dat$drive = NULL

#------ Remove features with constant values
constant = which(unlist(lapply(dat[,-ncol(dat)],function(x) all(x==x[1],na.rm=T))))
dat = dat[,-constant]


#------ Remove features with more than 10% NAs
many_nas = which(unlist(lapply(dat,function(x) sum(is.na(x))))> 10*nrow(dat)/100)
names(dat)[many_nas]
dat = dat[,-many_nas]

#----- Remove rows that are left with one or more NAs
idx = -which(is.na(dat), arr.ind=TRUE)[,1]
dat = dat[idx,]
distraction = distraction[idx]
speed = speed[idx]
acceleration = acceleration[idx]
lane = lane[idx]
steering = steering[idx]
subjects = subjects[idx]
aggressiveness = aggressiveness[idx]
drive = drive[idx]

print(paste0("Columns left from NA and linear dependence:",ncol(dat)))


#---------Normalize and remove variables that are highly correlated with others
dat = data.frame(sapply(dat,normalize))
means = dat[,grepl("mean",names(dat))]
dat = dat[,-which(grepl("mean",names(dat)))]
dat = cbind(means,dat)

cor_mat = cor(dat,use="p")
cor_mat[!upper.tri(cor_mat)] = 0
highly_cor = as.numeric(which(apply(cor_mat,2,function(x) any(abs(x)>.8))))
removed = names(dat)[highly_cor]

names(dat) = gsub("cov","acov",names(dat))
names(dat) =gsub("corr","acorr",names(dat))
dat = dat[ , order(names(dat), decreasing = TRUE)]
names(dat) = gsub("acov","cov",names(dat))
names(dat) =gsub("acorr","cor",names(dat))
highly_cor = which(names(dat) %in% removed)
cor_mat = cor(dat,use="p")
cor_mat[!upper.tri(cor_mat)] = 0
diag(cor_mat)=NA


kept = categories = names(dat)
kept[kept %in% names(dat)[highly_cor]]= ""
cor_mat[lower.tri(cor_mat)] = 0

gg = ggplot(aes(Var1,as.numeric(Var2) ), data=melt(cor_mat))+geom_tile(aes(fill=value),color="white") +
  scale_fill_gradient2(low="red",high="steelblue",limits=c(-1, 1))+labs(x="",y="")+
  scale_x_discrete(expand = c(0, 0))+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y=element_text(size=20,face="bold",angle=45),
        legend.title = element_blank(),
        legend.position = c(0.9, 0.1),
        legend.justification = c(.9, .1),
        legend.key.size = unit(0.8,"cm"))


for(sensor  in c("Perinasal","Breath","Palm","Heart")){
  centr_idx = tail(which(grepl(tolower(sensor),categories)),1)+.5
  gg= gg +geom_segment(x=0,xend=centr_idx,y=centr_idx,yend=centr_idx)
  categories[grepl(tolower(sensor),categories)]=""
  categories[round(centr_idx)] = sensor
}

centr_idx =  tail((which(grepl("cov|cor",categories))),1)+.5
gg = gg+geom_hline(yintercept=centr_idx)
categories[grepl("cov|cor",categories)]=""
categories[round(centr_idx)] = "Correlative"

gg + scale_y_continuous(breaks=seq_along(categories),
                        labels = categories)

ggsave("../Figures/correlation.png")


#----------------- Barplot of what is removed and what is kept for each signal
dat = dat[,-highly_cor]

intermediate = c(sum(grepl("perinasal",names(dat))),sum(grepl("breath",names(dat))),
  sum(grepl("palm",names(dat))),sum(grepl("heart",names(dat))),sum(grepl("cov|cor",names(dat))))

print(paste0("Columns left from correlations:",ncol(dat)))


#----- Normalization and feature selection based on XGBoost and linear models for distraction
temp_dat = dat
temp_dat$label = distraction
label = "distraction"
xgb_distraction_names = xgb_feature_selection(temp_dat, label)
linear_distraction_names = linear_feature_selection(temp_dat,label)



temp_dat$label = aggressiveness
label = "aggressiveness"
xgb_aggresiveness_names = xgb_feature_selection(temp_dat,label)
linear_aggresiveness_names = linear_feature_selection(temp_dat,label)

dist_f = union(xgb_distraction_names,linear_distraction_names)
agg_f = union(xgb_aggresiveness_names,linear_aggresiveness_names)
length(intersect(agg_f,dist_f))/length(agg_f)

print(xgb_distraction_names)
print(linear_distraction_names)
print(xgb_aggresiveness_names)
print(linear_aggresiveness_names)
chosen = c("acceleration","steering",union(union(union(xgb_distraction_names,linear_distraction_names),xgb_aggresiveness_names),linear_aggresiveness_names))


final = c(sum(grepl("perinasal",chosen)),sum(grepl("breath",chosen)),
                 sum(grepl("palm",chosen)),sum(grepl("heart",chosen)),sum(grepl("cov|cor",chosen)))

feature_barplot = data.frame(c("Perinasal","Breath","Palm","Heart","Connectivity"),
                             init, intermediate ,final)

names(feature_barplot)[1] = "Signals"
ggplot(aes(y=value,x=Signals,fill=variable),data=melt(feature_barplot))+
  geom_bar(stat="identity",position="dodge")+xlab("")+ylab("")+
  theme(legend.title = element_blank(),legend.text=element_text(size=13),axis.text.x=element_text(size=15))+
  scale_fill_manual(labels = c("Initial", 
                               "General",
                               "Task-specific"), 
                               values = c("steelblue3", "tan3","brown2"))

ggsave("../Figures/feature_reduction_bar.png")

print(paste0("Final number of columns:",length(chosen)))

#---------------------------------------------- 
dat$distraction = distraction
dat$aggressiveness = aggressiveness
dat$subject = as.factor(subjects)

dat$speed = normalize(speed)
dat$acceleration = normalize(acceleration)
dat$steering = normalize(steering)
dat$lane = normalize(lane)

dat$aggressiveness = aggressiveness
dat$drive = drive

write.csv(dat,"data_2_5.csv",row.names=F)
to_write = matrix(nrow=0,ncol=(length(chosen)*3+3))


#------ Each sample will forecast the value of the next sample, 
#------ so at the end of each drive of each subject, there will be an NA value
#------ and each sample will contain data from 3 lags back
for(s in unique(subjects)){
 
  sdat = dat[dat$subject==s,]
  
  normal_perinasal = as.numeric(unlist(sdat %>% filter(drive==1) %>% select(perinasal_mean)))
  
  for(d in unique(sdat$drive)){
    ddat = sdat[sdat$drive==d,]
    
    ddat = ddat[,c(chosen,"distraction","aggressiveness")]
    
    if(nrow(ddat)==0){
      next
    }
    
    #------------ Compute sympathetic arousal as extra output
    if(nrow(ddat)>length(normal_perinasal)){
       ddat$arousal = ceiling(c(ddat$perinasal_mean[1:length(normal_perinasal)] - normal_perinasal,
                                rep(NA,nrow(ddat)-length(normal_perinasal))))
       
    }else{
      ddat$arousal = ceiling(ddat$perinasal_mean - normal_perinasal[1:nrow(ddat)])
    }
    
    
    #------- Lag = 3
    ddat = data.frame(cbind(ddat[1:(nrow(ddat)-3),1:(ncol(ddat)-3)],
                            ddat[2:(nrow(ddat)-2),1:(ncol(ddat)-3)],
                            ddat[3:(nrow(ddat)-1),1:(ncol(ddat)-3)],
                            ddat[4:nrow(ddat),(ncol(ddat)-2):ncol(ddat)]))
    
    ddat$subject = s
    ddat$drive = d
    
    to_write = rbind(to_write,ddat)
  }
  
}
to_write = data.frame(to_write)
names(to_write) = names(ddat)

write.csv(to_write,"data_3.csv",row.names=F)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
print("----------------------------------------------------------------------------DONE feature selection")