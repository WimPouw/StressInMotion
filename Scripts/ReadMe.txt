MergingData_EasyAlign_and_Plotting.R This contains the most important procedure for merging trail data, execute easyalign, produce plots, and generate asynchrony dataset
ExtractTrialDataMTwavs.R takes as input trial times, experiment data and extracts motion tracking trial data and audio data
OpenPoseToTimeSeries.R extracts timeseries from raw Openpose data (also smooths the data)
falign_relative.praat this praat script is placed in the trialdata folder for each participant, via R we execute it
Praat (FOLDER) - to reproducable we have added praat with easyaling plugin. This way nothing needs to be installed to run the main R scripts