#---------------------------------------------------
# Plot the results of the predictions in their physical form
#   1. Load the dataset with the predictions of xgboost
#   2. Compute and store evaluation metric for each session
#   3. Plot the true positives, false positives and false negatives with different colors
#     in their physical form ( 10 second windows of each subject's recordings)
#---------------------------------------------------

libs = c("dplyr","ggplot2","caret","ROCR")
lapply(libs, require, character.only = TRUE)

setwd("../Data")

dat = read.csv("data_5.csv")

#------ compute precision of each drive
res = matrix(nrow=0,ncol=6)
for(d in unique(dat$drive)[2:4]){
  print(d)
  sd  = dat[dat$drive==d,c("aggressiveness","aggressiveness_act")]
  print(sum(sd$aggressiveness)/nrow(sd))
  conf_mat = table(dat[dat$drive==d,c("distraction","distraction_act")])
  specif = conf_mat[1,1]/sum(conf_mat[,1])
  sensiv = conf_mat[2,2]/sum(conf_mat[,2])
  preci =  conf_mat[2,2]/sum(conf_mat[2,])
  npv =    conf_mat[1,1]/sum(conf_mat[1,])
  pred = prediction(dat$distraction_act[dat$drive==d],dat$distraction[dat$drive==d])
  acc = sum(dat$distraction_act[dat$drive==d] == dat$distraction[dat$drive==d])/sum(dat$drive==d)
  res = rbind(res,c(slot(performance(pred,"auc"),"y.values"),acc,specif,sensiv,preci,npv))
}
write.csv(res,"Results/results_by_session.csv",row.names=F)

dat %>% group_by(drive) %>% summarise(n_distinct(subject))

#------------------------------ Remove normal and final driver for the paper plots
dat = dat%>%dplyr::filter(drive!=1, drive!=5)

dat$drive[dat$drive==2]  = "Cognitive"
dat$drive[dat$drive==3]  = "Sensorimotor"
dat$drive[dat$drive==4]  = "Emotional"
dat$drive[dat$drive==1]  = "Normal"
dat$drive[dat$drive==5]  = "Final"

names(dat)
dat = dat %>% dplyr::select(subject,drive,distraction_prediction,distraction,distraction_act,
                     aggressiveness_prediction,aggressiveness,aggressiveness_act,
                     arousal_prediction,arousal,arousal_act)%>% 
  group_by(subject,drive) %>% mutate(Frame=1:n())


for(v in c("distraction","aggressiveness")){
  
  dat$Prediction = ""
  dat$Prediction[dat[,paste0(v,"_act")]==1 & dat[,v]==1] = "True Positive"
  dat$Prediction[dat[,paste0(v,"_act")]==1 & dat[,v]==0] = "False Positive"
  dat$Prediction[dat[,paste0(v,"_act")]==0 & dat[,v]==1] = "False Negative"
  dat = dat[-which(dat[,paste0(v,"_act")]==0 & dat[,v]==0),]

  
  #colors=c("#999999", "#56B4E9","#E69F00")
  colors=c("red", "black","green")
  
  for(d in unique(dat$drive)){
    d = unique(dat$drive)[4]
    dat %>% filter(drive == d) %>% ggplot(.,aes(x=factor(subject),y=Frame,color=factor(Prediction)))+geom_point()+
      xlab("Subjects")+ylab("Time Windows (10 sec each)")+ggtitle("")+
      scale_color_manual(values=colors)+
      theme_bw()+
      theme(legend.title = element_blank(),
            legend.position = "top",
            legend.text = element_text(size=17),
            axis.title =  element_text(size=17),
            axis.text = element_text(size=14))+
            guides(colour = guide_legend(override.aes = list(size=6)))

    ggsave(paste0("../Figures/",v,"_prediction_",d,".pdf"),width = 7, height = 5.5)
  }
  
  if(v=="distraction"){
    ggplot(data=dat,aes(x=factor(subject),y=Frame,color=factor(Prediction)))+geom_point()+
      xlab("Subjects")+ylab("Time Windows (10 sec each)")+ggtitle("")+
      scale_color_manual(values=colors)+theme_bw()+
      theme(legend.title = element_blank(),
            legend.position = "top",#c(0.8, 0.2),
            legend.text = element_text(size=19),
            axis.title =  element_text(size=17),
            axis.text.y = element_text(size=12),
            axis.text.x=element_text(size=8, angle=90),
            strip.text = element_text(size=17))+#xlab("")+
      xlab("")+
      facet_wrap(~drive)+guides(colour = guide_legend(override.aes = list(size=6)))
    
  }else{
    
    ggplot(data=dat,aes(x=factor(subject),y=Frame,color=factor(Prediction)))+geom_point()+
      xlab("Subjects")+ylab("Time Windows (10 sec each)")+ggtitle("")+
      scale_color_manual(values=colors)+theme_bw()+
      theme(legend.title = element_blank(),
            legend.position = "none",#c(0.8, 0.2),
            legend.text = element_text(size=19),
            axis.title =  element_text(size=17),
            axis.text.y = element_text(size=12),
            axis.text.x=element_text(size=8, angle=90),
            strip.text = element_text(size=17))+
      facet_wrap(~drive)+guides(colour = guide_legend(override.aes = list(size=6)))
    
  }

  ggsave(paste0("../Figures/",v,"_prediction_full.pdf"),width = 9, height = 5.5)
}

