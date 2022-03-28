

# Name directory
dirname$="/Users/carlawiksebarrow/Desktop/frics/analysis_vux/all_files_vux/22000/"
outdirname$ = "/Users/carlawiksebarrow/Desktop/frics/analysis_vux/praat_results/static/"
  

# Get file names ending with wav from directory
flist = Create Strings as file list: "wavs", dirname$ + "*.wav" 
nfiles = Get number of strings

#Outer for loop that goes through all words in tier one: for all files in the folder that end with *wav, also open corresponding TextGridfile

for i from 1 to nfiles
	# Select files and get nr of intervals in TextGrid
	selectObject: flist
	fname$ = Get string: i
	sound_id = Read from file: dirname$ + fname$
	sound_name$ = selected$ ("Sound", 1)
	appendInfoLine: "Processing ", sound_name$
	# Make ouput files and write a row with column titles to the result file:
	resultfile$ = outdirname$ + sound_name$ + ".txt"
	titleline$ = "speaker" + tab$ + "word" + tab$ + "trajectory" + tab$ + "fricative" + tab$ + "duration" + tab$ + "fric_intensity" + tab$ +"window_intensity" + tab$ + "vowel" + tab$ + "time" + tab$ + "M1" + tab$ + "M2" + tab$ + "M3" + tab$ + "M4" + tab$ + "peak" 
	writeFileLine: resultfile$, titleline$
	int_id = To Intensity: 100, 0, "yes"
	traj = 1
	textgrid_path$ = dirname$ + sound_name$ + ".TextGrid" 
		textgrid_id = Read from file: textgrid_path$
		n_int = Get number of intervals: 1
		# Loop thorugh all non-empty intervals in Tier 1, if non-empty, find interval on Tier 2 at start time (this is empty)
		# and get info about the next interval(fricative) + define points for measurement
		for j from 1 to n_int
			selectObject: textgrid_id
			label$ = Get label of interval: 1, j
			appendInfoLine: "   ", label$
			if label$ != ""
				windowlength = 0.02
				start_int1 = Get start time of interval: 1, j
				preceding = Get interval at time: 2, start_int1
				label_prec$ = Get label of interval: 2, preceding
				if label_prec$ = ""
					label_fric$ =  Get label of interval: 2, (preceding+1)
					label_vowel$ = Get label of interval: 2, (preceding+2)
					startpoint = Get start time of interval: 2, (preceding+1)
					endpoint = Get end time of interval: 2, (preceding+1)
					selectObject: int_id
					fric_int = Get mean: startpoint, endpoint, "energy"
					startpoint = startpoint + (windowlength/2)
					endpoint = endpoint - (windowlength/2)
					duration = endpoint-startpoint
					step = duration/14
					total_fric_duration = duration + windowlength
					# Loop through 15 points in fricative interval in second tier, extracts a Hann window centered 
					# around the 15 points and get spectral moments 1-4, (power =2, weighted by power spectrum)
					for ii from 0 to 14
 						frame_mid = startpoint + ii * step
						left_frame = frame_mid - (windowlength/2)
						right_frame = frame_mid + (windowlength/2)
						selectObject: sound_id
						win_id = Extract part: left_frame, right_frame, "Hanning", 1, "no"
						win_int = Get intensity (dB)
						spectra_id = To Spectrum: "yes"
 						m1 = Get centre of gravity: 2
	  	 				m2 = Get standard deviation: 2
 	 					m3 = Get skewness: 2
 	 					m4 = Get kurtosis: 2
						matr_name$ = "/Users/carlawiksebarrow/Desktop/frics/analysis_vux/praat_results/matrix/"+ fname$ + "_" + label$ + "_" + label_fric$ + "_" + string$(traj) + "_" + string$(ii) + ".txt"
						matr_id = To Matrix
						Save as matrix text file: matr_name$
						Remove
						selectObject: spectra_id
						ltas_id = To Ltas (1-to-1)
						peak = Get frequency of maximum: 700, 0, "none"
						selectObject: int_id
						point_int = Get mean: left_frame, right_frame, "energy"
						# Save results to textfile
						resultline$ = sound_name$ + tab$ + label$ + tab$ + string$(traj) + tab$ + label_fric$ + tab$ + fixed$(total_fric_duration, 3) + tab$ + fixed$(fric_int, 3) + tab$ + fixed$(win_int, 3) + tab$ + label_vowel$ + tab$ + string$(ii) + tab$ + fixed$(m1, 3) + tab$ + fixed$(m2, 3) + tab$ + fixed$(m3, 3) + tab$ + fixed$(m4, 3) + tab$ + fixed$(peak, 3) 
						selectObject: win_id, spectra_id, ltas_id
						Remove
						appendFileLine: resultfile$, resultline$
					endfor
					traj += 1
				endif
			endif
		endfor
		# Remove the TextGrid object from the object list
		selectObject: textgrid_id
		Remove
	# Remove the temporary objects from the object list
	selectObject: sound_id, int_id
	Remove
endfor
selectObject: flist
Remove

