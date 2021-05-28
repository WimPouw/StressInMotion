library(bioacoustics)   #audio processing
library(tuneR)          #audio processing
library(rPraat)         #praat functions
library(seewave)        #signal processing/timeseries
library(signal)         #signal processing/timeseries
library(rstudioapi)     #folder finding
library(dplR)           #smoothing
library(stringr)        #string manipulations
library(wrassp)         #audio processing
library(scales)         #rescaling
library(ggplot2)        #plotting
library(gridExtra)      #plotting extras
library(kza)            #smoothing
library(speakr)         #audio processing with PRAAT
library(readtextgrid)   #reading txt grids
library(zoo)            #timeseries functions

#QUESTIONS: wimpouw@gmail.com

#######################################################################FUNCTIONS
#MAIN FUNCTION
amplitude_envelope.extract <- function(locationsound, smoothingHz, resampledHz)
{
  snd <- snd.read(locationsound)                                #read the sound file into R
  hilb <- hilbert(snd$sig, f = snd$fs, fftw =FALSE)             #apply the hilbert on the signal
  env <- abs(hilb)                                              #complex modulus
  if(!is.na(smoothingHz))
  {
    #apply modulus
    env <- hanning(env, n =snd$fs/smoothingHz)           #smooth with a hanning window
    env[is.na(env)] <- 0   
  }
  #set undeterminable at beginning and end NA's to 0
  f <- approxfun(1:(snd$duration*snd$fs),env)                       #resample settings at desired sampling rate
  downsampled <- f(seq(from=0,to=snd$duration*snd$fs,by=snd$fs/resampledHz))  #resample apply
  return(downsampled[!is.na(downsampled)])
}

#########for F0 extraction
F0.extract <- function(locationsound, gender_fm, resampledHz, minf, minamp)
{
  F0 <- ksvF0(locationsound,windowShift=1000/resampledHz,toFile = FALSE, gender = gender_fm, minAmp = minamp, minF = minf)
  F0_vec <- F0$F0[,1]
  F0_time <- seq(from = attributes(F0)$startTime*1000, to = attributes(F0)$endRecord[1]*(1000/attributes(F0)$sampleRate), by = 1000/attributes(F0)$sampleRate )
  F0_df <- cbind.data.frame(round(F0_time), F0_vec)
  return( F0_df )
}

################################PRAAT function to produce script for easyalign

#GET RELEVANT FOLDERS
parentfolder <- paste0(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))  #what is the current folder
ppns <- list.files(paste0(parentfolder, "/PilotData/"), pattern = "PPN_*.")

#set praat location
options(speakr.praat.path = paste0(parentfolder, "/Scripts/Praat/Praat.exe"))  

#trialinfo
trialinfo <- read.csv(paste0(parentfolder, "/StimuliStructureInfo/SIM_itemsv2_LvM2.csv"))

#gender
genders <- c("f", "m")

#initialize a data set for asynchronies
DD <- data.frame()

#Weightings
weights <- c(0.33, 0.33, 0.33)

#loop through participants
p = 0 #particpant iterator
for(ppn in ppns) 
{ 
    p <- p +1            #participant iterator
    gender <- genders[p] #get gender for this participant
    script <-paste0(parentfolder, "/PilotData/", ppn, "/TrialData/falign_relative.praat") #activate praat script
    praat_run(script, package = "speakr", capture = FALSE) #run praat script
    #loop through all trials
    txtgrds <- list.files(paste0(parentfolder, "/PilotData/", ppn, "/TrialData/"), pattern = "*.TextGrid") #list txtgrd files
    #prepare a dataset
    D <- rbind(trialinfo, trialinfo) #each trial is performed twice
    D$condition <- "gesture"                  #set to gesture
    D$condition[1:nrow(D)/2] <- "nogesture"   #set half to no gesture
    D$ppn <- D$syllables_detected <- D$stressed_syllable <- D$time_beat <-
      D$time_stress <- D$time_syl_L1 <- D$time_syl_L2 <- D$asynchrony_L1L2 <-D$asynchrony <- D$correct <- D$stressed_mistimingL1L2 <- NA
    #loop through all txt.grids
    for(trs in unique(txtgrds)) #loop through txtgrids, merge, collect synchrony info, and plot
    {
    txtgrd <- paste0(parentfolder, "/PilotData/", ppn, "/TrialData/",trs) #identify txtgrid file
    #load in textgrid with praatr
    tg <- tg.read(txtgrd, encoding = "auto")#read txtgrid
    MT <- read.csv(paste0(str_remove(txtgrd, ".TextGrid"), "_MT.csv")) #load in motion tracking
    MT$time_ms <- MT$time_ms-min(MT$time_ms) #recenter time so that timeries starts at 0
    soundloc <- paste0(str_remove(txtgrd, ".TextGrid"), ".wav") #get the sound location
    namesamp <- str_remove(trs, ".TextGrid") #get the clean file name without extension
      #get the amplitude envelope
      env <- rescale(amplitude_envelope.extract(soundloc, 10, 200), to = c(0,1)) #compute amplitude envelope wav
      time <- seq(from = 1000/200, to = length(env)*(1000/200), by = 1000/200)   #make a time vector
      envelope <- cbind.data.frame(time, env) #frame time and envelope
      
      #get F0
      namesamp2  <- iconv(namesamp,from="UTF-8", to="ASCII//TRANSLIT") #use the alternative naming as the nex function cant retrieve F0 otherwise
      soundloc2 <- paste0(parentfolder,"/PilotData/", ppn, "/TrialData/ASCIInames/", namesamp2, ".wav") 
      F0 <- F0.extract(soundloc2, gender, 200, 75, 50)      #extract F0
      F0$F0_vec[F0$F0_vec==0] <- NA
      F0$F0_vec <-  dplR::hanning(x = F0$F0_vec, n =200/40) #smooth 40 Hz Hanning
      F0$F0_vec[is.na(F0$F0_vec)] <- 0
      #speech (merge F0 and envelope, interpolate envelope where necessary)
      speechac <- merge(F0, envelope, by.x = "round(F0_time)", by.y = "time", all = TRUE)
      colnames(speechac) <- c("time_ms", "F0", "env")
      speechac$env <- na.approx(speechac$env, na.rm = FALSE)
      speechac                  <- speechac[!is.na(speechac$F0),] #only keep interpolated values
      
      #merge motion tracking with envelope, F0, and txt.grid
      MTe                  <- merge(MT, speechac, by.x = "time_ms", by.y = "time_ms", all = TRUE)
      MTe$z                <- na.spline(MTe$z, x= MTe$time_ms, na.rm = FALSE) #spline interpolation
      MTe$hand             <- unique(MTe$hand)[2] #get only non-na value
      MTe                  <- MTe[!is.na(MTe$env),-2] #only keep interpolated values
      colnames(MTe)        <- c("time_ms", "vertical_movement", "hand", "F0", "envelope")
      MTe$syllable_bs <- MTe$syllable_num <-     MTe$syllable_duration <- NA
      sylls     <- tg$syll$t1[tg$syll$label!="_"]
      syllsend  <- tg$syll$t2[tg$syll$label!="_"]
      for(i in 1:length(sylls)) #loop through syllable boundaries and add to timeseries
      {
        begin <- which.min(abs(MTe$time_ms-(sylls[i]*1000)))      #begin syllable
        end   <- which.min(abs(MTe$time_ms-(syllsend[i]*1000)))   #end syllable
        MTe$syllable_num[begin:end]   <- i
        MTe$syllable_duration[begin:end]  <-  sum(MTe$F0[begin:end]!=0)*5 #amount of milliseconds of vocalization
      }
      MTe$syllable_name <- tg$phono$label[2]
      
      #add which stress syllable is correct
      name  <- str_remove(namesamp, "_nogesture")  #get only the target name
      name  <- str_remove(name, "_gesture")        #get only the target name
      MTe$STRESS_L1 <-  MTe$STRESS_L2 <- NA
      MTe$stresstimeL1 <- MTe$stresstimeL2 <- NA
      MTe$STRESS_L1[MTe$syllable_num == trialinfo$stressed.syllable.L1[trialinfo$target==name]] <- "STRESS L1" 
      MTe$STRESS_L2[MTe$syllable_num == trialinfo$stressed.syllable.L2[trialinfo$target==name]] <- "STRESS L2" 
      
      #select the syllable that is stressed
      MTe$envelope_z <- MTe$duration_z <- MTe$F0_z <-MTe$STRESS<- MTe$stresstime <- NA
      MTe$envelope_z[!is.na(MTe$syllable_num)]  <- scale(MTe$envelope[!is.na(MTe$syllable_num)])[,1]
      MTe$duration_z[!is.na(MTe$syllable_num)]  <- scale(MTe$syllable_duration[!is.na(MTe$syllable_num)])[,1]
      MTe$F0_z[!is.na(MTe$syllable_num)]        <- scale(MTe$F0[!is.na(MTe$syllable_num)])[,1]
      #get maxima for
      maxenvelope_z <- ave(MTe$envelope_z, MTe$syllable_num, FUN= function(x) max(x)) #maximum envelope for each syllable
      maxF0_z       <- ave(MTe$F0_z, MTe$syllable_num, FUN= function(x) max(x))       #max F0 for each syllable
      MTe$stress_score <- (weights[1]*maxenvelope_z)+(weights[2]*maxF0_z)+(weights[3]*MTe$duration_z)
      MTe$STRESS[MTe$syllable_num==MTe$syllable_num[which.max(MTe$stress_score)]]       <- "STRESSED SYLLABLE"
      MTe$stresstime[which.max(MTe$envelope[MTe$STRESS=="STRESSED SYLLABLE"])] <- "STRESS"
      MTe$stresstimeL1[which.max(MTe$envelope[MTe$STRESS_L1=="STRESS L1"])] <- "L1 peak"
      MTe$stresstimeL2[which.max(MTe$envelope[MTe$STRESS_L2=="STRESS L2"])] <- "L2 peak"
      
      #get maximum extension
      MTe$peakz <- NA
      MTe$peakz[MTe$vertical_movement==min(MTe$vertical_movement[!is.na(MTe$syllable_num)])] <- "beat"
      
      #write merged motion tracking file to folder
      write.csv(MTe, paste0(parentfolder, "/PilotData/", ppn, "/Processed_TrialData/", namesamp, "_merged.csv"))
      
      ###################################Make a dataset for analysis
      #collect information from time series data
      condition <- str_remove(namesamp, paste0(name, "_"))
      indexD <- which(D$target==name & D$condition == condition)
      D$ppn[indexD] <- paste0("Pilot_PPN_", p)
      D$syllables_detected[indexD] <- max(MTe$syllable_num, na.rm = TRUE) #what is the number of easyalign detected syllables
      D$stressed_syllable[indexD]  <- max(MTe$syllable_num[MTe$STRESS=="STRESSED SYLLABLE"], na.rm = TRUE) #what is the syllable number thats stressed
      D$time_stress[indexD]        <- MTe$time_ms[!is.na(MTe$stresstime)] #time of stress
      D$time_beat[indexD]          <- MTe$time_ms[!is.na(MTe$peakz)]      #time of beat
      D$time_syl_L1[indexD]        <- MTe$time_ms[!is.na(MTe$stresstimeL1)] #time of L1 syllable occurence
      D$time_syl_L2[indexD]        <- MTe$time_ms[!is.na(MTe$stresstimeL2)] #time of L2 syllable occurence
      #raw asynchrony
      D$asynchrony[indexD]         <-  D$time_stress[indexD]-D$time_beat[indexD] #just capture the asynchrony
      #compute an asynchrony measure duch that negative values indicate overshoot in direction L2
      #and positive vlues indicate asynchrony towards L1 direction
      D$asynchrony_L1L2[indexD]             <- ifelse(D$L1.L2[indexD]!=0, 
                                                     ifelse(D$L1.L2[indexD]>0, (D$time_stress[indexD]-D$time_beat[indexD])*-1,(D$time_stress[indexD]-D$time_beat[indexD])),
                                                     D$time_stress[indexD]-D$time_beat[indexD])
      D$stressed_mistimingL1L2[indexD]      <- ifelse(D$L1.L2[indexD]!=0, 
                                             ifelse(D$L1.L2[indexD]>0, (D$time_stress[indexD]-D$time_syl_L2[indexD])*-1,(D$time_stress[indexD]-D$time_syl_L2[indexD])),
                                             D$time_stress[indexD]-D$time_syl_L2[indexD])
      if(D$stressed_syllable[indexD]==D$stressed.syllable.L1[indexD]){correct <- "incorrect_L1_match"}
      if(D$stressed_syllable[indexD]==D$stressed.syllable.L2[indexD]){correct <- "correct_L2_match"}
      if((D$stressed_syllable[indexD]!=D$stressed.syllable.L2[indexD])&(D$stressed_syllable[indexD]!=D$stressed.syllable.L2[indexD])){correct <- "incorrect_other"}
      D$correct[indexD] <- correct
      
      print(trs)
      print(D$condition[indexD])
      ################################################do some plotting and save each trial #uncomment if not necessary
      #MTes <- MTe[!is.na(MTe$syllable_num),]
      ##plot amplitude envelope
      #plotenv <- ggplot(MTes) + geom_path(aes(x=time_ms, y=envelope), color = "purple", size = 2) + 
      #  ggtitle(paste0(namesamp, "\n STRESS (peak envelope) = ", MTes$time_ms[!is.na(MTes$STRESS)], "milliseconds")) + 
      #  geom_vline(xintercept=MTes$time_ms[which(MTes$stresstime=="STRESS")], alpha = 0.3, size = 2) + theme_bw() 
      ##plot F0
      #MTes$F0[MTes$F0==0] <- NA
      #plotF0 <- ggplot(MTes) + geom_path(aes(x=time_ms, y=F0), color = "red", size = 2) + 
      #  ggtitle(paste0(namesamp, "\n STRESS (peak envelope) = ", MTes$time_ms[!is.na(MTes$STRESS)], "milliseconds")) + 
      #  geom_vline(xintercept=MTes$time_ms[which(MTes$stresstime=="STRESS")], alpha = 0.3, size = 2) + theme_bw() 
      ##plot MT
      #peaktime <- MTes$time_ms[!is.na(MTes$peakz)]
      #plotMT <- ggplot(MTes) + geom_path(aes(x=time_ms, y=vertical_movement), color = "black", size = 2) + ggtitle(paste0(namesamp, "\n maximum extension = ", peaktime, "milliseconds")) +
      #  theme_bw()
      #if(length(peaktime)>0)
      #{plotMT <-  plotMT+ geom_vline(xintercept=peaktime, alpha = 0.3, size = 2) }
      #
      ##get the waveform
      #rel_ac <- readWave(soundloc)
      #snd = rel_ac@left
      #times <-  seq(from = 1000/44100, to = length(snd)*(1000/44100), by = 1000/44100) 
      #snd = (snd - mean(snd))/1000
      #d <- cbind.data.frame(times, snd)
      #d <- d[seq(1, nrow(d), 40), ]
      #minploty <- min(d$snd)
      #maxploty <- max(d$snd)
      ##plot the waveform with some added information about stressed, L1 and L2 stress competitors
      #plotsound <- ggplot(d) + geom_path(aes(x=times, y=snd), color = "black", alpha = 0.5) + theme_bw()+ ggtitle(paste0(namesamp, "  (", paste0(unique(MTes$syllable_name), ") \n Intonation performance: ", correct)))+ 
      #  xlim(min(MTes$time_ms), max(MTes$time_ms))+
      #  geom_rect(data=d, aes(xmin=min(MTes$time_ms[!is.na(MTes$STRESS)]),xmax=max(MTes$time_ms[!is.na(MTes$STRESS)])),
      #            ymin= minploty,ymax=maxploty, fill=NA, color = "red", size=1, alpha=0.5, linetype = "dashed")+
      #  geom_rect(data=d, aes(xmin=min(MTes$time_ms[!is.na(MTes$STRESS_L2)]),xmax=max(MTes$time_ms[!is.na(MTes$STRESS_L2)])),
      #            ymin= minploty,ymax=maxploty, fill=NA, color = "green", size=0.5, alpha=0.5)+
      #  geom_rect(data=d, aes(xmin=min(MTes$time_ms[!is.na(MTes$STRESS_L1)]),xmax=max(MTes$time_ms[!is.na(MTes$STRESS_L1)])),
      #            ymin= minploty,ymax=maxploty, fill=NA, color = "blue", size=0.5, alpha=0.5, linetype = "dashed")+
      #  geom_text(aes(label ="STRESS L2", x = min(MTes$time_ms[!is.na(MTes$STRESS_L2)])+1, y =maxploty+1), color = "green", alpha = 0.5)+
      #  geom_text(aes(label ="STRESS", x = min(MTes$time_ms[!is.na(MTes$STRESS)])+1, y = 0), color = "red")+
      #  geom_text(aes(label ="STRESS L1", x = min(MTes$time_ms[!is.na(MTes$STRESS_L1)])+1, y = minploty+1), color = "blue", alpha = 0.5)
      #
      #print(paste0("working on plot for trial:", namesamp))
      #png(paste0(parentfolder,"/PilotData/", ppn, "/TrialData/Plots/", namesamp, ".png"),width=6,height=8,units="in",res=250)
      #if(D$condition[indexD]=="gesture"){print(grid.arrange(plotsound, plotenv, plotF0, plotMT, nrow = 4))}
      #if(D$condition[indexD]=="nogesture"){print(grid.arrange(plotsound, plotenv, plotF0, nrow = 3))}
      #dev.off()

    }
    DD <- rbind(DD, D)
}
#save the main timing data
write.csv(DD, paste0(parentfolder,"/ProcessedTimingData/DD.csv"))