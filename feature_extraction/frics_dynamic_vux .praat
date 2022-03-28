

# Name directory
dirname$="/Users/carlawiksebarrow/Desktop/frics/analysis_vux/all_files_vux/44100/"
outdirname$ = "/Users/carlawiksebarrow/Desktop/frics/analysis_vux/praat_results/dynamic/"
  

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
				windowlength = 0.015
				start_int1 = Get start time of interval: 1, j
				preceding = Get interval at time: 2, start_int1
				label_prec$ = Get label of interval: 2, preceding
				if label_prec$ = ""
					label_fric$ =  Get label of interval: 2, (preceding+1)
					startpoint = Get start time of interval: 2, (preceding+1)
					endpoint = Get end time of interval: 2, (preceding+1)
					startpoint = startpoint + (windowlength/2)
					endpoint = endpoint - (windowlength/2)
					duration = endpoint-startpoint
					step = duration/14
					total_fric_duration = duration + windowlength
					# Loop through 15 points in fricative interval in second tier, extracts a Hann window centered 
					# around the 15 points and get matrix
					for ii from 0 to 14
 						frame_mid = startpoint + ii * step
						left_frame = frame_mid - (windowlength/2)
						right_frame = frame_mid + (windowlength/2)
						selectObject: sound_id
						win_id = Extract part: left_frame, right_frame, "Hanning", 1, "no"
						spectra_id = To Spectrum: "yes"
						matr_name$ = "/Users/carlawiksebarrow/Desktop/frics/analysis_vux/praat_results/matrix/"+ fname$ + "_" + label$ + "_" + label_fric$ + "_" + string$(traj) + "_" + string$(ii) + ".txt"
						matr_id = To Matrix
						Save as matrix text file: matr_name$
						Remove
						selectObject: win_id, spectra_id
						Remove
					endfor
					traj += 1
				endif
			endif
		endfor
		# Remove the TextGrid object from the object list
		selectObject: textgrid_id
		Remove
	# Remove the temporary objects from the object list
	selectObject: sound_id
	Remove
endfor
selectObject: flist
Remove

