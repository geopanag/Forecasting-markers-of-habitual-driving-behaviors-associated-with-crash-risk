#---------------------------------------------------

#---------------------------------------------------

libs = c("dplyr","ggplot2","caret","ROCR")
lapply(libs, require, character.only = TRUE)

setwd("../Data")

dat = read.csv("data_5_3.csv")

#------ compute precision of each drive
res = matrix(nrow=0,ncol=6)
for(d in unique(dat$drive)[2:4]){
  print(d)
  conf_mat = table(dat[dat$drive==d,c("distraction","distraction_act")])
  specif = conf_mat[1,1]/sum(conf_mat[,1])
  sensiv = conf_mat[2,2]/sum(conf_mat[,2])
  preci =  conf_mat[2,2]/sum(conf_mat[2,])
  npv =    conf_mat[1,1]/sum(conf_mat[1,])
  pred = prediction(dat$distraction_act[dat$drive==d],dat$distraction[dat$drive==d])
  acc = sum(dat$distraction_act[dat$drive==d] == dat$distraction[dat$drive==d])/sum(dat$drive==d)
  res = rbind(res,c(slot(performance(pred,"auc"),"y.values"),acc,specif,sensiv,preci,npv))
}

#dat %>% group_by(drive) %>% summarise(n_distinct(subject))

#------------------------------ Remove normal and final driver for the paper plots
dat = dat%>%filter(drive!=1, drive!=5)

dat$drive[dat$drive==2]  = "Cognitive"
dat$drive[dat$drive==3]  = "Sensorimotor"
dat$drive[dat$drive==4]  = "Emotional"
dat$drive[dat$drive==1]  = "Normal"
dat$drive[dat$drive==5]  = "Final"


dat = dat %>% select(subject,drive,distraction,distraction_act)%>% 
  group_by(subject,drive) %>% mutate(Frame=1:n())


for(v in c("distraction")){
  
  dat$Prediction = ""
  dat$Prediction[dat[,paste0(v,"_act")]==1 & dat[,v]==1] = "True Positive"
  dat$Prediction[dat[,paste0(v,"_act")]==1 & dat[,v]==0] = "False Positive"
  dat$Prediction[dat[,paste0(v,"_act")]==0 & dat[,v]==1] = "False Negative"
  dat$Prediction[dat[,paste0(v,"_act")]==0 & dat[,v]==0] = "True Negative"
  
  colors=c("red", "black","black","green")
  shapes = c(16,16,1,16)
  
  if(v=="distraction"){
    ggplot(data=dat,aes(x=factor(subject),y=Frame,color=factor(Prediction)))+
      geom_point(aes(shape=factor(Prediction)),size=2)+
      xlab("Subjects")+ylab("Time Windows [1 min each]")+
      scale_color_manual(values=colors)+theme_bw()+
      scale_shape_manual(values=shapes) +ggtitle("A")+
      coord_fixed(ratio = 2)+
      theme(
        plot.title = element_text(size=14, face="bold"),
        legend.title = element_blank(),
            legend.position = "top",#c(0.8, 0.2),
            legend.text = element_text(size=19),
            axis.title =  element_text(size=17),
            axis.text.y = element_text(size=9),
            axis.text.x = element_blank(),#text(size=9, angle=90),
            axis.title.y = element_text(hjust = 0.1),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            strip.text = element_text(size=17))+#xlab("")+
      xlab("")+ scale_y_continuous(breaks = seq(1, 15, by = 1))+
      facet_wrap(~drive)+guides(colour = guide_legend(override.aes = list(size=6)))
  }
  
  
  ggsave(paste0("../Figures/new/",v,"_prediction_1_min.pdf"),width = 9, height = 5.5)
}

