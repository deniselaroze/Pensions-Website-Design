
################################
#### Emotions analyses 
#### author: Denise Laroze
################################
library(stargazer)
library(MASS)
library(broom)
library(ggpubr)
#library(naniar)
require(nnet)


rm(list=ls())


#path_datos <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Estudio Online/"
#path_datos <- "C:/Users/Profesor/Dropbox/Sitios web/Datos Estudio Online/"
path_datos <- "C:/Users/Denise/Dropbox/Sitios web/Datos Laboratorio/Videos/"


# If you don´t use Rprojects functionality setwd
#path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions-Website-Design/"
#path_github <- "C:/Users/Profesor/Documents/GitHub/Pensions-Website-Design/"
path_github <- "C:/Users/Denise/Documents/GitHub/Pensions Website Design/"


df<-read.csv(paste0(path_datos,"dataset_con_columnas.csv"))





#####################
### Balance test
####################


multinom_model1 <- multinom(Treatment ~ Gender , data = df)
stargazer(multinom_model1)

names(df)


table(df$treatment)
table(df$video)

