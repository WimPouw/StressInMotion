MergingData_EasyAlign_and_Plotting.R This contains the most important procedure for merging trail data, execute easyalign, produce plots, and generate asynchrony dataset
ExtractTrialDataMTwavs.R takes as input trial times, experiment data and extracts motion tracking trial data and audio data
OpenPoseToTimeSeries.R extracts timeseries from raw Openpose data (also smooths the data)
falign_relative.praat this praat script is placed in the trialdata folder for each participant, via R we execute it
markdown_prereg_analysis_SIM.Rmd - this is a markdown file with all the analysis (code) that will support our report
prereg_analysis_SIM.html - this is contains the knitted html output of the markdown file
Praat (FOLDER) - to reproducable we have added praat with easyaling plugin. This way nothing needs to be installed to run the main R scripts