library(rjson)        # used for loading json's
library(rstudioapi)   # used for pointing to folders relative to the folder you in
library(kza)          # smoothing

#Questions: Wim Pouw (wimpouw@gmail.com)

#for each keypoint n you get xn, yn, confidencen
#notes on keypoint ordering
##//     {0,  "Nose"},
#//     {1,  "Neck"},
#//     {2,  "RShoulder"},
#//     {3,  "RElbow"},
#//     {4,  "RWrist"},
#//     {5,  "LShoulder"},
#//     {6,  "LElbow"},
#//     {7,  "LWrist"},
#//     {8,  "MidHip"},
#//     {9,  "RHip"},
#//     {10, "RKnee"},
#//     {11, "RAnkle"},
#//     {12, "LHip"},
#//     {13, "LKnee"},
#//     {14, "LAnkle"},
#//     {15, "REye"},
#//     {16, "LEye"},
#//     {17, "REar"},
#//     {18, "LEar"},
#//     {19, "LBigToe"},
#//     {20, "LSmallToe"},
#//     {21, "LHeel"},
#//     {22, "RBigToe"},
#//     {23, "RSmallToe"},
#//     {24, "RHeel"},
#//     {25, "Background"}
#// };

##hand output see
##"https://github.com/CMU-Perceptual-Computing-Lab/openpose/blob/master/doc/output.md#hand-output-format"


#set parentfolders
basefolder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))     #get path of current R folder
list_ppnfol <- list.files(paste0(basefolder, "/PilotData/"), pattern = "PPN.*")   #list the videofolders present

#handeddnes
handedness <- c("left", "right") #pilotdata (Lieke is left, Wim is a righty)

#loop for each video output folder through all json frames
for(vidfold in list_ppnfol)
  {
  tsoutput <- paste0(basefolder, "/PilotData/", vidfold, "/OpenPoseData/")                        #write folder for the time series
  jsfileloc <- paste0(basefolder, "/PilotData/", vidfold, "/OpenPoseData/jsons/")    #location json files
  list_json <- list.files(  jsfileloc, pattern = ".json")      #index all json files
  MT <- data.frame()                                           #initialize a motion tracking data frame
  print(paste0("processing: ", vidfold))                       #inform about progress
  
  for(frame in list_json)
  {
    #load jsd$pose_keypoints_2d
    jsd <- fromJSON(file = paste0(  jsfileloc, frame))         #load in frame
    if(length(jsd$people[]) > 0 )
    {
    #get nose points, keypoint 0
    x_nose <- jsd$people[[1]]$pose_keypoints_2d[1]       
    y_nose <- jsd$people[[1]]$pose_keypoints_2d[2]
    conf_nose <- jsd$people[[1]]$pose_keypoints_2d[3]
    #get hand wrist points, keypoint 0
      #left hand
    x_wrist_left <- jsd$people[[1]]$hand_left_keypoints_2d[1]
    y_wrist_left <- jsd$people[[1]]$hand_left_keypoints_2d[2]
    conf_wrist_left  <- jsd$people[[1]]$hand_left_keypoints_2d[3]
     #right hand
    x_wrist_right <- jsd$people[[1]]$hand_right_keypoints_2d[1]
    y_wrist_right <- jsd$people[[1]]$hand_right_keypoints_2d[2]
    conf_wrist_right  <- jsd$people[[1]]$hand_right_keypoints_2d[3]
    #get index points, keypoint 8
      #left hand
    x_index_left <- jsd$people[[1]]$hand_left_keypoints_2d[22]
    y_index_left <- jsd$people[[1]]$hand_left_keypoints_2d[23]
    conf_index_left  <- jsd$people[[1]]$hand_left_keypoints_2d[24]
      #right hand
    x_index_right <- jsd$people[[1]]$hand_right_keypoints_2d[22]
    y_index_right <- jsd$people[[1]]$hand_right_keypoints_2d[23]
    conf_index_right  <- jsd$people[[1]]$hand_right_keypoints_2d[24]
    
    #append frame info to dataframe
    frameMT <- cbind.data.frame(x_nose, y_nose,conf_nose, 
                     x_wrist_left, y_wrist_left, conf_wrist_left, 
                     x_wrist_right, y_wrist_right, conf_wrist_right, 
                     x_index_left, y_index_left, conf_index_left,
                     x_index_right, y_index_right, conf_index_right)
    MT <- rbind.data.frame(MT, frameMT)
      }
    }
  #make time
  MT$time_ms <- round(seq(1000/(50), (nrow(MT)*(1000/50)), by = 1000/50))         #set time vector to 50 frames/per second (but check your video)
  MT$y_wrist_left  <- MT$y_wrist_left*-1                  #mirror y as y-axis is flipped (higher position = lower y)
  MT$y_index_left  <- MT$y_index_left*-1                  #mirror y as y-axis is flipped (higher position = lower y)
  MT$y_wrist_right  <- MT$y_wrist_right*-1                #mirror y as y-axis is flipped (higher position = lower y)
  MT$y_index_right  <- MT$y_index_right*-1                #mirror y as y-axis is flipped (higher position = lower y)
  
  #make another slim dataframe for the hand and smooth with a Kolmogorov-Zurbenko filter (span 5, order 3)
  MT$y_index_left <- kz(MT$y_index_left, 5, 3) 
  MT$y_index_left <- kz(MT$y_index_right, 5, 3)
    
  if(handedness[which(list_ppnfol == vidfold)] =="left")    #based on on right or left handedness extract only relevant time series
  {MTslim <- cbind.data.frame(MT$time_ms, MT$y_index_left)
  MTslim$hand <- "left"}  
  if(handedness[which(list_ppnfol == vidfold)] =="right")
  {MTslim <- cbind.data.frame(MT$time_ms, MT$y_index_right) 
  MTslim$hand <- "right"}
  
  #save in time series output folder
  write.csv(MT, paste0(tsoutput, vidfold, "_MT.csv"))         #the timeseries with more body points
  write.csv(MTslim, paste0(tsoutput, vidfold, "_MTslim.csv")) #the slim version will be our main file
}

#########################for method section show an example time series
library(ggplot2)   #for plotting
library(gridExtra) #for combine plots
sample <- MT[3600:4900, ] #extract a sample from the motion tracking file
sample$time_ms <- (max(sample$time_ms)-sample$time_ms)/1000 #set start to 0 time
sample$smoothed <- kz(sample$y_index_left, 5, 3) #make a smoothed version
a <- ggplot(sample, aes(x=time_ms)) +geom_path(aes(y = smoothed), color = "red") + geom_rect(aes(xmin = 4, xmax = 6, ymin = -1500, ymax = -485), colour = "grey", fill = NA, alpha = 0.3)+
  geom_path(aes(y = y_index_right)) + theme_bw() #zoomed out plot
b <- ggplot(sample[sample$time_ms >4 & sample$time_ms < 6, ], aes(x=time_ms)) + geom_path(aes(y = y_index_right)) + geom_path(aes(y = smoothed), color = "red") + theme_bw()
grid.arrange(a,b)#zoomed in plot