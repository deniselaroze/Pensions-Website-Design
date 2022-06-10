###########################
## Pensions website Experiment
## Data Analysis
## Author: Denise Laroze
### May 2022
############################


library(naniar)
library(readr)
library(matrixStats)
library(BBmisc)
library(dplyr)

setwd("C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Piloto/Superintendencia de Pensiones/online")

rm(list=ls())

load("Pilot_data.Rdata")

df<-encuestas

# Data Manipulation

web<-as.factor(df$website)
df$treat<-recode(web, 
                 "1" = "Profile",
                 "2" = "Video",
                 "3" = "VideoProfile",
                 "4" = "Profile",
                 "5" = "Video",
                 "6" = "VideoProfile",
                 "7" = "Baseline",
                 "8" = "Baseline"
)


######



table(df$treat, df$correct_response)

names(df)
## reached the end



###########
ggline(pilot_data, x = "Treat", y = "correct_response", add = "mean_se",
       color = "Gender", palette = "jco") +
  xlab("Treatments") +
  ylab("Number of Clicks") +
  ggpubr::stat_compare_means(aes(group=Gender), label = "p.signif", method = "wilcox.test") +
  scale_y_continuous(limits = c(0,50)) 
  theme_gppr()














#### OLD ##############
## Respuestas correctas
df %>%
  group_by(treat) %>%
  summarise_at(vars(correct_response), list(mean = mean, sd= sd), na.rm=T)


## info abrumadora por tratamiento
df %>%
  group_by(treat) %>%
  summarise_at(vars(InfoAbruma2_1), list(mean = mean, sd= sd), na.rm=T)

## info Util
df %>%
  group_by(treat) %>%
  summarise_at(vars(InfoUtil_1), list(mean = mean, sd= sd), na.rm=T)


## Curiosity
df %>%
  group_by(treat) %>%
  summarise_at(vars(Curiosity_1), list(mean = mean, sd= sd), na.rm=T)




##Confidence
df %>%
  group_by(treat) %>%
  summarise_at(vars(Confidence_1), list(mean = mean, sd= sd), na.rm=T)












names(dv)

