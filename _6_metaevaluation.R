compute_results <- function(sdat){
  acc = sum(sdat[,1] == sdat[,2])/nrow(sdat)
  conf_mat = table(sdat)
  specif = conf_mat[1,1]/sum(conf_mat[,1])
  sensiv = conf_mat[2,2]/sum(conf_mat[,2])
  preci =  conf_mat[2,2]/sum(conf_mat[2,])
  npv =    conf_mat[1,1]/sum(conf_mat[1,])
  
  return(c(acc,specif,sensiv,preci,npv))
}

library(ggplot2)
library(dplyr)
library(zoo)
library(ROCR)

setwd("../Data")
set.seed(2018)

dat = read.csv("data_4_2.csv")

threshold = 0.4
df = dat
df$to_plot=ifelse(df$distraction==0, "No", "Yes")
ggplot(df,aes(distraction_prediction,fill=factor(to_plot)))+geom_density(alpha=.5)+  #,aes(y=..scaled..)
  guides(fill=guide_legend(title="Distraction"))+xlab("Predicted probability of distraction")+ylab("Density")+
  theme(axis.title=element_text(size=17),axis.text=element_text(size=15),axis.text.y=element_blank(),axis.ticks.y = element_blank())+
  geom_vline(xintercept=threshold,col="magenta")

ggsave("../Figures/new/Figure5.pdf")
df$to_plot=NULL

#---- One minute window (6 step)
labels = c()
dist = c()
drives = c()
subjects = c()
for( s in  unique(dat$subject)){
  for( d in  unique(dat$drive)){
    t = dat %>% dplyr::filter(drive==d, subject==s) %>% dplyr::select(distraction,distraction_prediction)
    if(nrow(t)>0){
      
      #t$distraction_prediction = shift(t$distraction_prediction, n=1, fill=0, type="lead")
      lab = ifelse(rollapply(t$distraction, 6, mean,by=6)>=0.5,1,0)
      act = rollapply(t$distraction_prediction, 6, mean,by=6)
      
      labels = c(labels,lab)  
      dist = c(dist,act)
      drives = c(drives,rep(d,length(act)))
      subjects = c(subjects,rep(s,length(act)))
    }
  }
}

pred = prediction(dist,labels)


#----- with 0.4 threshold
dist_2 = ifelse(dist>=0.4,1,0)
sdat = data.frame(cbind(dist_2,labels,drives,subjects))
names(sdat) = c("distraction_act","distraction","drive","subject")
write.csv(sdat,"data_5_3.csv",row.names = F)

sdat = data.frame(cbind(dist_2,labels))
results = c(unlist(slot( performance(pred,"auc"),"y.values")),compute_results(sdat))
res_class2 = unlist(lapply(results*100,function(x) format(round(x,2),nsmall=2)))

#----- with 0.3 threshold
dist_1 = ifelse(dist>=0.3,1,0)
sdat = data.frame(cbind(dist_1,labels))
results = c(unlist(slot( performance(pred,"auc"),"y.values")),compute_results(sdat))
res_class1 = unlist(lapply(results*100,function(x) format(round(x,2),nsmall=2)))

library(xtable)
old  = c("84.26","78.36","87.28","61.47","71.83","81.10")
dat = cbind(old,res_class1,res_class2)
dat = data.frame(dat)
names(dat) = c("Old","0.3 threshold","0.4 threshold")
xtable(dat)

#------------------------------------


sdat = data.frame(cbind(dist,labels))
names(sdat) = c("distraction_prediction","distraction")

df = sdat
df$to_plot=ifelse(df$distraction==0, "No", "Yes")
ggplot(df,aes(distraction_prediction,fill=factor(to_plot)))+geom_density(alpha=.5)+  #,aes(y=..scaled..)
  guides(fill=guide_legend(title="Distraction"))+xlab("Predicted probability of distraction")+ylab("Density")+
  theme(axis.title=element_text(size=17),axis.text=element_text(size=15))

ggsave("../Figures/new/Figure5b.pdf")
df$to_plot=NULL


