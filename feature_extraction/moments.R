library('seewave')
library('stringr')

input_dir <- "/Users/carlawiksebarrow/Desktop/frics/analysis_vux/praat_results/matrix/"
files <- list.files(path= input_dir, pattern = "*.txt")
paths <- paste0(input_dir, files)

moments <- function(path, samp_freq, frame_dur){
  spec <- read.csv(path, sep = ' ', head = FALSE, skip = 4)
  frame_size <- 2^ceiling(log2(frame_dur * samp_freq))
  bin_width <- samp_freq / frame_size

  spec_pow <- as.numeric(spec[1,])^2 + as.numeric(spec[2,])^2
  spec_pow_db <- 10 * log10(spec_pow)
  freqs <- 0:(length(spec_pow) - 1) * bin_width
  freqs_mel <- mel(freqs)
  # plot(freqs_mel, spec_pow_db, type = 'l')
  
  m1 <- mean(freqs_mel * spec_pow) / mean(spec_pow)
  c2 <- mean((freqs_mel - m1)^2 * spec_pow) / mean(spec_pow)
  m2 <- sqrt(c2)
  c3 <- mean((freqs_mel - m1)^3 * spec_pow) / mean(spec_pow)
  m3 <- c3 / c2^1.5
  c4 <- mean((freqs_mel - m1)^4 * spec_pow) / mean(spec_pow)
  m4 <- c4 /c2^2 - 3
  
  bin_cutoff <- which.min(abs(freqs_mel - mel(1000))) #this needs to be changed - for sje and possibly also for tje
  
  spec_pow_filt <- spec_pow[bin_cutoff:length(spec_pow)]
  spec_peak <- freqs_mel[which.max(spec_pow_filt) + bin_cutoff - 1]
  fname <- str_replace(str_replace(basename(path), '.txt', ''), '_ch1.wav', '')
  metadata <- unlist(str_split(fname, "_"))
  
  return(list(metadata[[1]], metadata[[2]], metadata[[3]], metadata[[4]], metadata[[5]],
              m1, m2,m3, m4, spec_peak))
}

# moments('/Users/carlawiksebarrow/Desktop/frics/analysis_vux/praat_results/matrix/8D_ch1.wav_sil_s_136_10.txt',
#         44100, 0.02)
data <- t(sapply(paths, moments, samp_freq=44100, frame_dur=0.015))
colnames(data) <- c('speaker', 'word', 'fricative', 'trajectory', 'time', 'm1', 'm2', 'm3', 'm4', 'spectral_peak')
rownames(data) <- NULL
write.csv(data, "/Users/carlawiksebarrow/Desktop/frics/analysis_vux/mel_all_files44100.csv", row.names = FALSE)

