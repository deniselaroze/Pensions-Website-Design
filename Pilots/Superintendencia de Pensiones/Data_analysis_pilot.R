###########################
## Pensions website Experiment
## Data Analysis
## Author: Denise Laroze
### May 2022
############################




setwd("C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Piloto/Superintendencia de Pensiones")

rm(list=ls())

load("Pilot_data.Rdata")

table(df$treat, df$correct_response)


## reached the end




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

