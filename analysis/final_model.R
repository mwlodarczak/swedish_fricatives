# plots in mel + final model

library("tidyverse")
library("knitr")
library("seewave")
library("mgcv")
library("itsadug")

frics <- read.csv("/Users/carlawiksebarrow/Desktop/frics/analysis_vux/mel_all_files44100_durs.csv", encoding="utf-8")

#organize appropriately
frics$gender <- ifelse(frics$speaker %in% c('101C','102D','103A','104B','105C','106D','107A', '108B', '109C', '111D'), "male", "female") 
frics <- frics[frics$fricative %in% c('s', 'tj', 'sj'),]
frics$fricative <- factor(frics$fricative, levels=c("s", "tj", "sj")) #just to get the right order
frics$vowel <- ifelse(frics$word %in%
                        c('fat', 'sal', 'tjat', 'sjal'), 'A', ifelse(frics$word %in%
                        c('fot', 'sol', 'kjol', 'sjok'), 'O', ifelse(frics$word %in%
                        c('fall', 'saft', 'tjafs', 'schack'), 'a', ifelse(frics$word %in%
                        c('fil', 'sil', 'kil', 'skida'), 'I',ifelse(frics$word %in%
                        c('ful', 'sur', 'tjur', 'sjuk'), 'U', ifelse(frics$word %in%
                        c('fel', 'zebra', 'kedja', 'sked'), 'E', ifelse(frics$word %in%
                        c('skjorta'), 'o', 'e')))))))
frics <- frics[frics$vowel %in% c('A','I','O', 'E', 'a', 'e', 'U'),]
frics$fricative <- as.factor(frics$fricative)
frics$time <- as.numeric(frics$time)
frics$speaker <- as.factor(frics$speaker)
frics$vowel <- as.factor(frics$vowel) 
frics$gender <- as.ordered(frics$gender)
frics$word<- as.factor(frics$word) 
frics$trajectory <- as.factor(frics$trajectory)
frics$lip_round <- ifelse(frics$vowel %in%
                            c('U', 'O'), 'rounded',
                          ifelse(frics$vowel %in%
                                   c('I','E'), 'spread',
                                 'other'))
frics$lip_round <- as.factor(frics$lip_round)
frics$log_dur <- log2(frics$dur) 

frics_filt <- frics %>%
  group_by(fricative, time, speaker) %>%
  mutate(m1_z = (m1 - mean(m1)) / sd(m1)) %>%
  group_by(speaker, trajectory) %>%
  mutate(is_outlier = any(abs(m1_z) > 3)) %>%
  filter(is_outlier == FALSE)

frics_filt$start.event <- frics_filt$time == 0

#plot of trajectories, two speakers
frics_speaker <- frics_filt[frics_filt$speaker %in% c('106D', '111D'),]

ggplot(frics_speaker, aes(x=time, y=m1,  color=fricative, group=trajectory))+
  theme_light()+
  facet_grid(speaker~ fricative)+
  geom_line(alpha=0.5)+
  scale_color_manual(values =c("s"="grey48", "tj"="grey28", "sj"="grey12")) +
  labs(y="M1 (mel)", x ="Time, normalized")+
  theme(strip.background = element_blank(), strip.text = element_text(colour = 'black'), legend.position="none")+
  labs(color = element_blank())


#plot of trajectories
frics_filt$speaker_traj <- interaction(frics_filt$speaker, frics_filt$trajectory)

ggplot(frics_filt, aes(x = time, y = m1, color=fricative, group=speaker_traj)) +
  theme_light()+
  geom_line(alpha = 0.4)+
  facet_wrap( ~ fricative)+ 
  scale_colour_manual(values=c("s"="grey48", "tj"="grey28", "sj"="grey12"))+
  labs(y="M1 (mel)", x ="Time, normalized")+
  theme(strip.background = element_blank(), strip.text = element_text(colour = 'black'), legend.position="none")+
  labs(color = element_blank())
 

### MODEL

#model without AR1 error model:
m6 <- bam(m1 ~ fricative +
            s(time, by=fricative)+
            s(trajectory, bs="re") +
            s(word, bs="re")+
            s(time, by=lip_round) +
            ti(time, log_dur) +
            s(time, speaker, bs="fs")+
            s(time, speaker, by=fricative, bs="fs"),
          data=frics_filt, method="fREML",
          family="scat", discrete=TRUE)

gam.check(m6)
sum_m6 <- summary(m6)
resids_m6 <- acf_resid(m6, split_pred="trajectory")


#model with AR1 error model:
m6.AR <- bam(m1 ~ fricative + 
               s(time, by=fricative)+
               s(trajectory, bs="re") +
               s(word, bs="re")+
               s(time, by=lip_round) +
               ti(time, log_dur) +
               s(time, speaker, bs="fs")+
               s(time, speaker, by=fricative, bs="fs"),
             data=frics_filt, method="fREML", rho=resids_m6[2],
             AR.start=frics_filt$start.event, family="scat", discrete=TRUE)


gam.check(m6.AR)
sum_m6.AR <- summary(m6.AR)
resids_m6.AR <- acf_resid(m6.AR, split_pred="AR.start")



### TABLE 
gamtabs(sum_m6.AR)



### PLOTS

#fricatives
fric_palette <- c("grey48", "grey28", "grey12")
fric_col
plot(m6.AR)
plot_smooth(m6.AR, view="time", plot_all="fricative", rug=F, rm.ranef=T, n.grid=100, xlab="Time, normalized", ylab="M1 (mel)", col=fric_col_palette, alpha=0.5, hide.label = TRUE)
text(14.3, 850, 'sj')
text(14.3, 1800, 's')
text(14.3, 1600, 'tj')


#duration
log2(.125)
log2(.250)
plot_smooth(m6.AR, view="time", cond=list(fricative="s", log_dur=-2),rug=F, col="grey48", xlab="Time, normalized", ylab="M1 (mel)", ylim=c(500,3000), hide.label = TRUE, alpha=0.4)
plot_smooth(m6.AR, view="time", cond=list(fricative="s", log_dur=-3),rug=F, col="grey48",lty = "dashed", add=T, hide.label = TRUE, alpha=0.4)
plot_smooth(m6.AR, view="time", cond=list(fricative="tj", log_dur=-2),rug=F, col="grey28", add=T, hide.label = TRUE, alpha=0.4)
plot_smooth(m6.AR, view="time", cond=list(fricative="tj", log_dur=-3),rug=F, col="grey28",lty = "dashed", add=T, hide.label = TRUE, alpha=0.4)
plot_smooth(m6.AR, view="time", cond=list(fricative="sj", log_dur=-2),rug=F, col="grey12", add=T, hide.label = TRUE, alpha=0.4)
plot_smooth(m6.AR, view="time", cond=list(fricative="sj", log_dur=-3),rug=F, col="grey12", lty = "dashed",add=T, hide.label = TRUE, alpha=0.4)
text(14.3, 850, 'sj')
text(14.3, 1800, 's')
text(14.3, 1600, 'tj')


#lip rounding
plot_smooth(m6.AR, view="time", cond=list(fricative="s", lip_round="other"),rug=F, lty="solid", xlab="Time, normalized", ylab="M1 (mel)")
plot_smooth(m6.AR, view="time", cond=list(fricative="s", lip_round="rounded"),rug=F, lty="dotted", add=T)
plot_smooth(m6.AR, view="time", cond=list(fricative="s", lip_round="spread"),rug=F, lty="dashed", add=T)


plot_smooth(m6.AR, view="time", cond=list(fricative="tj", lip_round="other"),rug=F, lty= "solid", xlab="Time, normalized", ylab="M1 (mel)", hide.label = TRUE)
plot_smooth(m6.AR, view="time", cond=list(fricative="tj", lip_round="rounded"),rug=F, lty= "dotted",add=T, hide.label = TRUE)
plot_smooth(m6.AR, view="time", cond=list(fricative="tj", lip_round="spread"),rug=F, lty= "dashed", add=T, hide.label = TRUE)

plot_diff(m6.AR, view="time", comp=list(lip_round=c("other", "rounded")), cond=list(fricative="sj"), xlab="measurement number", ylab="Est. difference in M1 (Hz)", main=expression("Diff duration")) 
plot_diff(m6.AR, view="time", comp=list(lip_round=c("other", "spread")), cond=list(fricative="sj"), xlab="measurement number", ylab="Est. difference in M1 (Hz)", main=expression("Diff duration")) 
plot_diff(m6.AR, view="time", comp=list(lip_round=c("spread", "rounded")), cond=list(fricative="sj"), xlab="measurement number", ylab="Est. difference in M1 (Hz)", main=expression("Diff duration")) 

plot_smooth(m6.AR, view="time", cond=list(fricative="sj", lip_round="other"),rug=F, lty= "solid", xlab="Time, normalized", ylab="M1 (mel)", hide.label = TRUE)
plot_smooth(m6.AR, view="time", cond=list(fricative="sj", lip_round="rounded"),rug=F, lty="dotted", add=T, hide.label = TRUE)
plot_smooth(m6.AR, view="time", cond=list(fricative="sj", lip_round="spread"),rug=F, lty="dashed", add=T, hide.label = TRUE)




