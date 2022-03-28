library("tidyverse")
library("psych")
library("knitr")

#import
setwd("/Users/carlawiksebarrow/Desktop/frics/analysis_vux/praat_results/static/")
files <- list.files(pattern = "*.txt")
frics_stat <- do.call(rbind, 
                      lapply(files, function(x) read.csv(x,sep = '\t'))) 

#organize
frics_stat$gender <- ifelse(frics_stat$speaker %in% c('101C_ch1','102D_ch1','103A_ch1','104B_ch1','105C_ch1','106D_ch1','107A_ch1', '108B_ch1', '109C_ch1', '111D_ch1'), "male", "female") 
frics_stat <- frics_stat[frics_stat$fricative %in% c('f', 's', 'sj', 'tj'),] 
frics_stat$fricative <- factor(frics_stat$fricative, levels=c("f", "s", "tj", "sj")) 
frics_stat <- frics_stat[frics_stat$vowel != "o",] 

frics_stat <- frics_stat %>% 
  group_by(speaker) %>%
  mutate(z_intensity = (fric_intensity - mean(fric_intensity)) / sd(fric_intensity),
         z_win_intensity = (window_intensity - mean(window_intensity)) / sd(window_intensity))

frics_stat <- frics_stat %>% 
  group_by(speaker, time) %>%
  mutate(z_ind_win_intensity = (window_intensity - mean(window_intensity)) / sd(window_intensity))

#only the one middle point:
stat_frics_stat <- frics_stat[frics_stat$time %in% c('8'),]
stat_frics_stat$log_dur <- log2(stat_frics_stat$duration)

#exploring relationships in pairs panel
pairs.panels(stat_frics_stat[c(10:14, 17:18)],
            gap = 0,
            bg = c("black", "#E69F00", "#56B4E9", "#009E73")[stat_frics_stat$fricative],
            pch = 21)


### TABLES
#spectral measures
frics_stat_means <- stat_frics_stat %>%
  group_by(fricative, gender) %>%
  summarize(m1_mean = round(mean(M1),0),
            m1_sd =round(sd(M1),0),
            m2_mean = round(mean(M2),0),
            m2_sd =round(sd(M2),0),
            m3_mean = round(mean(M3),2),
            m3_sd =round(sd(M3),2),
            m4_mean = round(mean(M4),2),
            m4_sd=round(sd(M4),2),
            peak_mean = round(mean(peak),0),
            peak_sd =round(sd(peak),0))

kable(frics_stat_means, format="latex", caption="Measures averaged across vowel context, for females and males.")

#duration and intensity
frics_stat_means2 <- stat_frics_stat %>%
  group_by(fricative, gender) %>%
  summarize(dur_mean = round(mean(as.numeric(duration)), 3),
            dur_sd = round(sd(as.numeric(duration)), 3),
            z_int_mean =round(mean(as.numeric(z_intensity)),2),
            z_int_sd = round(sd(as.numeric(z_intensity)),2))

kable(frics_stat_means2, format="latex", caption="Fricative duration and z-scored intensity, averaged across vowel context, for females and males.")



### PLOTS
#m1-m2
ggplot(stat_frics_stat, aes(x=M1, y=M2, colour=fricative))+
  theme_light()+
  xlab("M1 (Hz)") +
  ylab("M2 (Hz)")+
  geom_point(aes(group=fricative), alpha=0.8)+
  theme(strip.background = element_blank(), strip.text.x = element_blank(), legend.position="top")+
  labs(color = element_blank())+
  scale_colour_manual(values=c("f"="grey64", "s"="grey48", "tj"="grey28", "sj"="grey12"))
# for colours, from colour-blind appropriate palette: "f"="#0072B2", "s" = "#E69F00","tj" = "#56B4E9","sj" = "#009E73"

#intensity
frics_stat$speaker_traj <- interaction(frics_stat$speaker, frics_stat$trajectory)
ggplot(frics_stat, aes(x=time, y=z_win_intensity, group=speaker_traj, colour=fricative))+
  theme_light()+
  ylab("Intensity, normalized")+
  xlab("Time, normalized")+
  geom_line(alpha=0.7)+
  facet_grid(~fricative)+
  theme(strip.background = element_blank(), strip.text = element_text(colour = 'black'), legend.position="none")+
  scale_colour_manual(values=c("f"="grey64", "s"="grey48", "tj"="grey28", "sj"="grey12"))
  

 #other visualization of intensity
ggplot(stat_frics_stat, aes(y=fric_intensity, x=fricative))+
  geom_boxplot()+
  facet_wrap(~speaker)


            


