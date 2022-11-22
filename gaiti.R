# LOAD PACKAGES
library(pacman)
p_load(tidyverse, ggpubr, rstatix, lme4) 

#START HERE
#import gait data
gait <- read_csv("gait.csv")
df <- as_tibble(gait)

#gather data
df <- df %>%
  dplyr::mutate(subject = row_number()) %>%
  gather(key="slip", value = "MOS", 1:12) %>%
  separate(slip, c("Deformity","Condition"), sep = "([._:])")

####STATS####

##ANOVA two-way interactions
res.aov <- anova_test(
  data = df, dv = MOS, wid = subject,
  within = c(Deformity, Condition))
get_anova_table(res.aov)

##Effect of treatment at each time point
one.way <- df %>%
  group_by(Deformity) %>%
  anova_test(dv = MOS, wid = subject, within = Condition) %>%
  get_anova_table()
one.way

##Pairwise comparisons between treatment groups
pwc <- df %>%
  group_by(Deformity) %>%
  pairwise_t_test(
    MOS ~ Condition, paired = TRUE,
    p.adjust.method="none"
  )
pwc

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#summary data for plotting
tgc <- summarySE(df, measurevar="MOS", groupvars=c("Deformity","Condition"))
tgc

# 95% confidence intervals geom_bar
ggplot(tgc, aes(x=Condition, y=MOS)) + 
  geom_bar(position=position_dodge(), stat="identity", fill = "#5576D1",) +
  geom_errorbar(aes(ymin=MOS-ci, ymax=MOS+ci),
                width=.2,                   
                position=position_dodge(.9)) +
  facet_wrap(vars(Deformity)) +
  theme_classic()

#clean up
rm(list = ls())
cat("\014")  
dev.off()
