#----------------------------------------------------------------------------
# Read the dataset as is from .osf and tansform it for the analysis
#   1. Load the dataset from each subject and each drive
#   2. Filter the signals with erroneous values (stemming from sensor problems etc.) based on validity index table from quality control 
#   3. Remove NAs
#   4. Resample all signals to 1Hz
#   5. Cut all signals to the same length
#   6. Use the .stm file from the experimenter to define distracted windows
#   7. Combine and store everything in one data frame simulation_1.csv
#----------------------------------------------------------------------------

libs = c("signal","dplyr","xlsx","TTR")
lapply(libs, require, character.only = TRUE)

#---------- Set the folder from osf
setwd("../Data/raw")

##--------- Substitute impossible physiological values with the mean
clean <- function(rec,down,up){
  
  rec[rec<=down||rec>=up]=NA
  if(sum(rec<=down,na.rm=T)>0 || sum(rec>=up,na.rm=T)>0){
    return(0)  
  }
  
  return(rec)
}


##---------- Use validity index table from quality control to remove erroneous subjects or recordings
quality_idx = read.csv("../Dataset-Table-Index.csv")
idx = quality_idx[which(quality_idx$pp==1 & quality_idx$peda==1 & quality_idx$HR==1 & quality_idx$BR==1 & quality_idx$RES==1),c("Subject","Session")]

sim1_data = matrix(ncol=12,nrow=0)


##--------- Iterate through subjects and drives to retrieve the data
for(subject_folder in 1:length(dir(full.names=T))){
  s_path = dir(full.names=T)[subject_folder]
  
  valid_drives = idx[idx$Subject == gsub("./","",s_path),2]
  
  subject = as.numeric(gsub("./T0","",s_path))
  
  print(subject)
  
  for(drive_folder in 1:length(dir(s_path,full.names=T))){
    d = dir(s_path,full.names=T)[drive_folder]
    
    drive = strsplit(d," ")[[1]][2]
    
    print("drive")
    print(d)
    
    #------ Check if the recording exists and if it is validated
    if(!(dir.exists(d) & (drive %in% valid_drives) )){
      next
    }
    
    #-------- Keep only Cognitive, Motoric and Normal Drive
    if((grepl("BL",d)|grepl("PD",d))){
      next
    }
    
    
    #--------- Read breathing and heart (they have the same frequency)
    if(length(dir(d,pattern="*.BR",full.names=T))>0){
      br = data.frame(sapply(
        read.xlsx2(dir(d,pattern="*.BR",full.names=T),1,startRow=9,stringsAsFactors=FALSE)
        ,as.numeric))
      hr = data.frame(sapply(
        read.xlsx2(dir(d,pattern="*.HR",full.names=T),1,startRow=9,stringsAsFactors=FALSE)
        ,as.numeric))
      
      breathing= br[,3]
      heart = hr[,3]
      time = hr[,2]
      
      breathing = clean(breathing,4,40)
      heart  = clean(heart,40,120)
      
      if(sum(breathing)==0){
        next
      }
    }
    
    
    ##-------- Read perinasal perspiration
    if(length(dir(d,pattern="*.pp",full.names=T))>0){
      pp = data.frame(sapply(
        read.xlsx2(dir(d,pattern="*.pp",full.names=T),1,startRow=9,stringsAsFactors=FALSE)
        ,as.numeric))
      
      if(is.na(pp$Time[1])){
        pp = data.frame(sapply(
          read.xlsx(dir(d,pattern="*.pp",full.names=T),1,startRow=9,stringsAsFactors=FALSE)
          ,as.numeric))
      }
      
      pp[,1]=NULL
      pp[,2]=NULL
      
      # Filter the data for NAs
      pp = pp[,colSums(is.na(pp)) < nrow(pp)-1]
      pp = pp[complete.cases(pp),]
      
      perinasal = c()
      # Resample to 1hz
      for(i in 1:ceiling(max(pp$Time))){
        perinasal = c(perinasal,mean(pp[pp$Time>(i-1)&pp$Time<i,2]))
      }
      
    }else{
      perinasal = rep(NA,length(heart))
    }
    
    
    ##----------- Read palm EDA
    if(length(dir(d,pattern="*.peda",full.names=T))>0){
      ped = data.frame(sapply(
        read.xlsx2(dir(d,pattern="*.peda",full.names=T),1,startRow=9,stringsAsFactors=FALSE)
        ,as.numeric))
      ped$Frame=NULL
      
      
      peda = c()
      # Resample to 1hz
      for(i in 1:ceiling(max(ped$Time))){
        peda = c(peda,mean(ped[ped$Time>(i-1)&ped$Time<i,2]))
      }
      peda= clean(peda,28,628)
      
    }else{
      peda = rep(NA,length(heart))
    }
    
    
    
    ##----------- Read vehicle data
    if(length(dir(d,pattern="*.res",full.names=T))>0){
      re = data.frame(sapply(
        read.xlsx2(dir(d,pattern="*.res",full.names=T),1,startRow=9,stringsAsFactors=FALSE)
        ,as.numeric))
      re$Frame=NULL
      re$Lane.Offset=NULL
      re$Braking=NULL
      
      res=matrix(nrow=0,ncol=4)
      # Resample to 1hz
      for(i in 1:ceiling(max(re$Time,na.rm=T))){
        res = rbind(res,sapply(re[re$Time>(i-1)&re$Time<i,2:5],mean,na.rm=T))
      }
      
      res = res[complete.cases(res),]
      
    }else{
      res =  matrix(ncol=4,nrow=length(heart))
    }
    
    ##-----------  Cut all signals to the same length
    minimum_length = min(c(length(breathing),length(heart),length(peda),length(perinasal),nrow(res)))
    if(minimum_length<100){
      next
    }
    
    
    extracted = data.frame(cbind(
      time[1:minimum_length],
      res[1:minimum_length,],
      breathing[1:minimum_length],
      heart[1:minimum_length],
      peda[1:minimum_length],
      perinasal[1:minimum_length]))
    
    names(extracted) = c("Time","Speed","Acceleration","Steering","Lane.Position","Breathing","Heart","Palm","Perinasal")
    
    
    ##----------- Set the distraction indicator signals
    extracted$Distraction = 0
    
    ##----------- Read the start and end of distracted signal
    if(length(dir(d,pattern="*.stm",full.names=T))>0){
      distracted = data.frame(sapply(
        read.xlsx2(dir(d,pattern="*.stm",full.names=T),1,startRow=9,stringsAsFactors=FALSE)[1:2,1:2]
        ,as.numeric))
      
      
      if(grepl("FD",d)){
        extracted$Drive = 5
        
        ##------- Keep only the time before failure accident and set the first distraction            
        extracted = extracted[1:which.min(abs(extracted$Time-distracted$EndTime[1])),]
        extracted$Distraction[which.min(abs(extracted$Time-distracted$StartTime[1])):nrow(extracted)] = 1
        
      }else{
        
        if(distracted$EndTime[2]>max(extracted$Time) | distracted$StartTime[1]<min(extracted$Time)){
          print("Error in distraction timestamps")
          next
        }
        
        ##------- Find the idx of time in the extracted data that is closer to the respective timestamps 
        start_distraction = c(which.min(abs(extracted$Time-distracted$StartTime[1])),
                           which.min(abs(extracted$Time-distracted$StartTime[2])))
        
        end_distraction = c(which.min(abs(extracted$Time-distracted$EndTime[1])),
                         which.min(abs(extracted$Time-distracted$EndTime[2])))
        
        extracted$Distraction[start_distraction[1]:end_distraction[1]] = 1
        extracted$Distraction[start_distraction[2]:end_distraction[2]] = 1
        
        if(grepl("CD",d)){
          extracted$Drive = 2
        }else if(grepl("MD",d)){
          extracted$Drive = 3
        }else if(grepl("ED",d)){
          extracted$Drive = 4
        }
      }
      
    }else{
      if(grepl("ND",d)){
        extracted$Drive = 1
      }else if(grepl("RD",d)){
        extracted$Drive = 0
      }
    }
    
    extracted$Subject = subject
    sim1_data = rbind(sim1_data,as.matrix(extracted))
   
  }

}


sim1_data = data.frame(sim1_data)
names(sim1_data)=c("Time","Speed","Acceleration", "Steering","Lane.Position","Breathing","Heart","Palm","Perinasal","Distraction","Drive","Subject")


sim1_data = sim1_data[complete.cases(sim1_data),]

smoothing_window = 5

#SMA(sim1_data$Heart,n=smoothing_window,na.rm=T)

moving_average <- function(x,n){
  new = x
  for(i in 1:(length(x)-n+1)){
    new[i] = mean(x[i:(i+n-1)],na.rm=T)
  }
  new
}

#------------------ Smooth all phisiological variables in every recording
dat_smoothed = sim1_data %>% group_by(Subject,Drive) %>% mutate(
  Heart = moving_average(Heart,n=smoothing_window),#,na.rm=T),
  Perinasal = moving_average(Perinasal,n=smoothing_window),#,na.rm=T),
  Breathing = moving_average(Breathing,n=smoothing_window),#,na.rm=T),
  Palm = moving_average(Palm,n=smoothing_window))#,na.rm=T))

dat = dat_smoothed[complete.cases(dat_smoothed),]

write.csv(dat_smoothed,"../data_1.csv",row.names=F)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
print("----------------------------------------------------------------------------DONE Creating the  dataset")
