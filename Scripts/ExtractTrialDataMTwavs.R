library(bioacoustics)  #audioprocessing
library(tuneR)      #audioprocessing
library(rPraat)     #praat audio functions
library(seewave)    #signal processing
library(signal)     #signal processing
library(rstudioapi) #for folder identification
library(dplR)    #data wrangling
library(stringr) #string manipulations
library(wrassp)  #F0 extraction
library(scales)  #rescaling
library(kza)     #smoothing

#questions: wimpouw@gmail.com

#GET RELEVANT FOLDERS
parentfolder <- paste0(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))  #what is the current folder
ppns <- list.files(paste0(parentfolder, "/PilotData/"), pattern = "PPN_*.")

#######################################################################FUNCTIONS
#MAIN FUNCTION
amplitude_envelope.extract <- function(locationsound, smoothingHz, resampledHz)
{
  snd <- snd.read(locationsound)                                #read the sound file into R
  hilb <- hilbert(snd$sig, f = snd$fs, fftw =FALSE)             #apply the hilbert on the signal
  env <- abs(hilb)
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

#########################################################
ppnt <- fasttiming <- vector()

    ##########STEP 1: get begin and end time times from the beep data
for(ppn in ppns) #loop through the ppn file folders
{
  wavef <- paste0(parentfolder, "/PilotData/", ppn, "/Audio/",ppn, "_beep.wav") #This is the folder where your wav's are saved
  
    #extract amplitude envelope of the beep data
    sampling <- 100 #sampling rate 100Hz
    env <- amplitude_envelope.extract(wavef,NA, sampling)
    envelope <- cbind.data.frame(env)
    envelope$time <- seq(from = 1000/sampling, to = nrow(envelope)*(1000/sampling), by = 1000/sampling)
    envelope$env = (envelope$env -min(envelope$env ))/(max(envelope$env )-min(envelope$env )) #rescale to min max
    write.csv(envelope, paste0(parentfolder, "/PilotData/", ppn, "/Experimentdata/audio_data_trialtimes.csv"))
      #read
      envelope <- read.csv(paste0(parentfolder, "/PilotData/", ppn, "/Experimentdata/audio_data_trialtimes.csv"))
    
    trialboundary <- envelope$time[envelope$env > 0.25] #when sound exceeds a 25% treshold record time
    d <- c(0, diff(trialboundary))                #check when there is a change in the trialboundary
    time <- cbind.data.frame(trialboundary, d)  #collect in one dataset
    time <- time[time$d > 1000 | d== 0, ]       #only keep trialboundaries
    write.csv(time, paste0(parentfolder, "/PilotData/", ppn, "/Experimentdata/trialtimes.csv"))
}   
    #####################STEP 2: Save Motion tracking trials

for(ppn in ppns) #loop through participant folders
{  
    #read trialdata, experimentdata, and motion tracking files
    met <- read.csv(paste0(parentfolder, "/PilotData/", ppn, "/Experimentdata/",ppn, ".csv"))
    MT <- read.csv(paste0(parentfolder, "/PilotData/", ppn,  "/OpenPoseData/", ppn, "_MTslim.csv"))
    time <- read.csv(paste0(parentfolder, "/PilotData/", ppn, "/Experimentdata/trialtimes.csv"))
    n <- 0
    trialtime <- c(diff(time$trialboundary), 3200) #determine the trailtimes
    for(tt in time$trialboundary)
    {
      n <- n+1  #iterator
      MTtr <- MT[ (MT$MT.time_ms > tt & MT$MT.time_ms < (tt)+trialtime[n]) , ] #extract from motion tracking the trialdata
      namesamp <- met$target[n]     #get the target word
      cond     <- met$condition[n]  #get the condition
      colnames(MTtr) <- c("frame", "time_ms","z", "hand")
      #save the motion tracking for this trial
      write.csv(MTtr, paste0(parentfolder,"/PilotData/", ppn, "/TrialData/", namesamp, "_", cond, "_MT.csv"), row.names = FALSE)
    }
}

     #STEP 3 loop through timings read from large wav file and save trial wav files
ppnt <- fasttiming <- vector()
for(ppn in ppns) #loop through participant folders
{   
  met <- read.csv(paste0(parentfolder, "/PilotData/", ppn, "/Experimentdata/",ppn, ".csv"))
  wavesp <- paste0(parentfolder,"/PilotData/", ppn, "/Audio/" ,ppn, "_speech.wav") #This is the folder where your wav's are saved
  met$timingf0 <-  met$timingenv <- NA
    #####################STEP 3: Save TrialData
    time <- read.csv(paste0(parentfolder, "/PilotData/", ppn, "/Experimentdata/trialtimes.csv"))
    n <- 0
    trialtime <- c(diff(time$trialboundary), 3200) #determine the trailtimes
    for(tt in time$trialboundary)
    {
      n <- n+1
      #collect sample and trial info
      samp <- read_wav(wavesp,time_exp = 1, from = tt/1000, to = (tt/1000)+(trialtime[n]/1000))
      namesamp <- met$target[n]
      cond <-  met$condition[n]
      #save files with ASCII name name
      namesamp2  <- iconv(namesamp, to="ASCII//TRANSLIT")
      print(paste0(namesamp2, namesamp))
      soundloc <- paste0(parentfolder,"/PilotData/", ppn, "/TrialData/", namesamp, "_",   cond, ".wav") 
      soundloc2 <- paste0(parentfolder,"/PilotData/", ppn, "/TrialData/ASCIInames/", namesamp2,"_",   cond, ".wav") 
      writeWave(samp, soundloc, extensible = TRUE)
      writeWave(samp, soundloc2, extensible = TRUE)
    }
}
