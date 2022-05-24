###########################
## Pensions website Experiment
## Data Analysis
## Author: Denise Laroze
### May 2022
############################




setwd("C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Piloto/Superintendencia de Pensiones")

rm(list=ls())

load("Pilot_data.Rdata")

table(df$website)
table(df$treat)

table(df$treat, df$total_reward)
table(df$treat, df$InfoAbruma2_1)
