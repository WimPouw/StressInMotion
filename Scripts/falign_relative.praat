# Hans Rutger Bosker (C)
# HansRutger.Bosker@mpi.nl
# 14 Dec, 2020
dirin$ = ".\"

Create Strings as file list... sounds 'dirin$'\*.wav
nsounds = Get number of strings

#for i from 1 to 3
for i from 1 to 'nsounds'
	select Strings sounds
	fileplusext$ = Get string... 'i'
	extposition = index(fileplusext$, ".wav")
	nameplus$ = left$(fileplusext$, ('extposition'-1))
	underscoreposition = index(nameplus$, "_")
	name$ = left$(nameplus$, ('underscoreposition'-1))

	Create Strings as tokens... 'name$' ;
	Rename... item_'i'
	Read from file... 'dirin$'\'nameplus$'.wav
	Convert to mono
	Rename... item_'i'
	
	plus Strings item_'i'
	runScript: "..\..\..\Scripts\Praat\plugin_easyalign\utt_seg2.praat", "ortho", "no"

	select TextGrid item_'i'
	runScript: "..\..\..\Scripts\Praat\plugin_easyalign\phonetize_orthotier2.praat", "ortho", "phono", "spa", "yes", "no"

	select Sound item_'i'
	plus TextGrid item_'i'
	runScript: "..\..\..\Scripts\Praat\plugin_easyalign\align_sound.praat", "ortho", "phono", "yes", "spa", "}-';(),.?¿", "no", "yes", "no", 90, "yes", "no"
	
	select TextGrid item_'i'
	Write to text file... 'dirin$'\'nameplus$'.TextGrid
endfor