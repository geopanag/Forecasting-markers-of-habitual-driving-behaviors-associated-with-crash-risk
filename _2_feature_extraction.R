#--------------------------------------------------------------------------------
# Extract features in time windows
#     1. Load preprocessed dataset simulation_1.csv
#     2. For each recording (Subjet and Driving session)  run a sliding window and extract features:
#     4. Statistical
#     5. Temporal
#     6. Structural
#     7. Correlative 
#     8. Spectral
#     9. Mean  driving signals
#     10. Threshold based binary transformation on aggregation and steering window
#     11. Define aggresiveness based on the transformations in 10
#     11. Store the data in simulation_2.csv
#--------------------------------------------------------------------------------

libs = c("signal","dplyr","pastecs","ggplot2","caret","zoo","tibble","lomb")
lapply(libs, require, character.only = TRUE)
#--------------------------------------------------

#---------------------------- Statistical features

statistical_features = function(x){
  #----------------- Statistical metrics from x
  l = summary(lm(x ~ c(1:length(x))))
  c(mean(x),
    median(x),
    sd(x),
    sum(x^2),
    l$coef[2]/l$coef[4]
  )
}




#--------------------------------------------------- Time series features
temporal_features = function(x){
#------------------------ Autocorrelation features from x
  if(var(x)<0.0000000000001){
    coef = NA
    res = NA
  }else{
    model = arima(x)
    coef= as.numeric(model$coef)
    res = sum(model$residuals^2)
  }
  
  c(# parameters of autocorrelation function 
     acf(ts(x),plot=FALSE)$acf[1:10],
     #  arima coeffiicient
     coef,
     #  arima residuals
     res
  )
}
x = rep(c(1:3),4)
arima(x)

correlation_features = function(subdat){
  #------------------------ Correlation between all signals in window
  cormat = cor(subdat)
  c(# upper triangle of correlation matrix
    cormat[upper.tri(cormat)],
    # eigen values of covariance matrix
    svd(cov(subdat))$d)
}



#-------------------------------------- Structural features
threshold <-function(x){
  (mean(x)-max(x))/2
}

find_peaks <-function (x, thresh = 0) {
  #-------- Peak and onset detection
  # Define the peaks as the places where the first derivative crosses zero
  # To refrain from extracting close peaks, set a threshold over allowed difference of the peaks
  # Define the onset of peak i as the lowest point betwen peak i and peak i-1
  #----------------------------------------------------------------------------
  pks = which(diff(sign(diff(x))) < 0) + 2
  pks = pks[x[pks - 1] - x[pks] > thresh]
  pks = pks-1
  if(length(pks)<2){
    if(length(pks)==0){
      return(data.frame(pks=0,onsets=0))
    }
    onsets = which.min(x[1:(pks-1)])
    return(data.frame(pks = pks,onsets = onsets))
  }
  onsets = c(1)
  for(j in 2:length(pks)){
    onsets = c(onsets, pks[j-1]+which.min(x[(pks[j-1]+1):(pks[j]-1)]))
  }
  for(i in 1:length(onsets)){
    if(onsets[i]==0 | onsets[i] %in% pks){
      onsets[i]=onsets[i]+1
    }
  }
  return(data.frame(pks = pks,onsets = onsets))  
}



structural_features = function(x){
  #---------------------- Metrics based on peak detection on x
  
  x = as.numeric(x)
  peaks_and_onsets = find_peaks(x,threshold(x))
  if(any(is.na(peaks_and_onsets$pks))){
    c(0,0,0,0)
  }
  c(# number of peaks
    length(peaks_and_onsets$pks),
    #avg diff between peaks and onsets
    mean(x[peaks_and_onsets$pks]-x[peaks_and_onsets$onsets]),
    #avg time diff between peaks and onsets
    mean(peaks_and_onsets$pks-peaks_and_onsets$onsets),
    #avg time diff between consecutive peaks
    ifelse(is.na(mean(diff(peaks_and_onsets$pks))),0,mean(diff(peaks_and_onsets$pks)))
  )
}




#------------------------------------------------- Spectral features
deriveSpec = function(dat,band){
  as.numeric(dat %>% 
               dplyr::filter(freq>band[1],freq<=band[2]) %>% 
               summarise(energy = sum(power^2)))
}



perspiration_features <-function(x){
  #-------- Extraction of spectral features using Lomb periodogram
  f = spectrum(x, plot =FALSE)
  
  c(# first peak 
    max(f$spec[f$freq<=0.2]),
    # first peak's frequency
  f$freq[which.max(max(f$spec[f$freq<=0.2]))],
  # second peak 
  max(f$spec[f$freq>0.2]),
  # second peak's frequency
  f$freq[which.max(max(f$spec[f$freq>0.2]))])
}



heart_features <- function(x){
  #-------- Extraction of spectral features using Lomb periodogram
  if(all(x==x[1]) | any(is.na(x))){
    return(c(0,0,0,0,0))
  }
  
  lomb = lsp(x, plot =FALSE)
  lomb = data.frame(lomb$scanned,lomb$power)
  names(lomb) = c("freq","power")
  # High frequency component
  HF = deriveSpec(lomb,c(.15,0.5))
  # Low frequency component
  LF =deriveSpec(lomb,c(.04,0.15))
  # Ratio of the above
  rat = LF/HF
  # Very Low frequency component
  VLF =deriveSpec(lomb,c(.003,0.04))
  # Total power
  TP= sum(lomb$power^2)
  c(HF,LF,VLF,rat,TP)
}



breathing_features <- function(x){
  #-------- Extraction of spectral features using Welch's average periodogram
  # Power spectral density in
  # 0-0.1, 0.1-0.2, 0.2-0.3, 0.3-0.4 Hz
  #-----------------------------------------------------------
  
  if(all(x==x[1]) | any(is.na(x))){
    return(c(0,0,0,0))
  }
  #--------------------- Breathing spectrum with hanning window
  breath_spec = specgram(x) #,n=window,1)
  breath_spec = data.frame(freq = breath_spec$f/2,power = Re(breath_spec$S))
  
  
  c(deriveSpec(breath_spec,c(0,0.1)),
               deriveSpec(breath_spec,c(0.1,0.2)),
               deriveSpec(breath_spec,c(0.2,0.3)),
               deriveSpec(breath_spec,c(0.3,0.4)))
}


#--------------------------------------------- Main
window = 10
setwd("../Data")

#-------------------------------------- Main
#------- Define the names of the final dataset
name = c("mean",  "median",  "sd",  "energy","rate", 
          paste0("acf",1:(window)),  "arima_param",  "arima_resid",  "peaks_no",  "peaks_onset_diff",
         "peaks_onset_time", "peaks_time")

name = c(paste0("perinasal_",name),paste0("heart_",name),paste0("breathing_",name),paste0("palm_",name),
         "corr_per_hr","corr_per_br","corr_per_pa","corr_hr_br","corr_hr_pa","corr_pa_br",paste0("cov_eigen",1:4),
         "perinasal_max_spec1","perinasal_max_freq1","perinasal_max_spec2","perinasal_max_freq2",
         "palm_max_spec1","palm_max_freq1","palm_max_spec2","palm_max_freq2",
         "heart_high","heart_low","heart_very_low","heart_ratio","heart_total_power",
         "breath_very_low","breath_low","breath_high","breath_very_high","subject","drive","distraction",
         "speed","acceleration","steering","lane","aggressiveness")


#----------------------------------------
dat = read.csv("data_1.csv")
dat$Steering = abs(dat$Steering)
dat = data.frame(dat %>% dplyr::filter(!(Subject %in% c(6,8)) & Drive != 0 ))

#---------- Acceleration  density threshold
d = density(dat$Acceleration)
ts_y = ts(d$y)
tp = turnpoints(ts_y)
d$x[tp$tppos]
acceleration_threshold = 2.5
ggplot(dat,aes(Acceleration))+geom_density()+
  xlab("Density")+ylab(expression(paste("Acceleration ["^"o","]")))+
  theme(axis.title=element_text(size=20),axis.text=element_text(size=15))+
  geom_vline(xintercept=acceleration_threshold,col="red")

ggsave("../Figures/acceleration_density.pdf")

to_write = matrix(ncol = length(name),nrow=0)

steering_threshold = mean(dat$Steering,na.rm=T)+sd(dat$Steering,na.rm=T)

for(s in unique(dat$Subject)){
  print(s)
  
  for(d in unique(dat$Drive)){
    
    sdat = dat[dat$Subject==s & dat$Drive==d,]
    
    if(nrow(sdat)==0){
      next
    }
    win = seq(1,nrow(sdat),window)
    
    for(i in 2:length(win)){
      win_dat = sdat[win[i-1]:(win[i]-1),]  
      
      new_row = c()
      for(col in c("Perinasal","Heart","Breathing","Palm")){

        nv = c(statistical_features(win_dat[,col]),
              temporal_features(win_dat[,col]),
              structural_features(win_dat[,col]))

        new_row = c(new_row,nv)
      }
      
      
      #--------------------------- Cluster steering time series with DTW
      new_row = c(new_row,
                correlation_features(win_dat[,c("Perinasal","Heart","Breathing","Palm")]),
                perspiration_features(win_dat$Perinasal),
                perspiration_features(win_dat$Palm),
                heart_features(win_dat$Heart),
                breathing_features(win_dat$Breathing),
                s,d, round(mean(win_dat$Distraction,is.na=T)),
                mean(win_dat$Speed,is.na=T),
                mean(win_dat$Acceleration,is.na=T),
                mean(win_dat$Steering,is.na=T),
                mean(win_dat$Lane.Position,is.na=T),
                as.numeric(ifelse(any(win_dat$Steering>=steering_threshold),1,0) & ifelse(any(win_dat$Acceleration>=acceleration_threshold),1,0)))
      
      if(length(new_row)!=length(name)){
        print("STOP")
        stop()
      }
      to_write = rbind(to_write,new_row)
      
    }
    
  }
} 

to_write = data.frame(to_write)

names(to_write) = name

write.csv(to_write,"data_2.csv",row.names=F)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
print("----------------------------------------------------------------------------DONE feature extraction")
