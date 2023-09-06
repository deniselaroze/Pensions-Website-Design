
################################
#### Emotions analyses 
#### author: Denise Laroze and Mauricio López
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
path_datos <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Laboratorio/Videos/"


# If you don´t use Rprojects functionality setwd
#path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions-Website-Design/"
#path_github <- "C:/Users/Profesor/Documents/GitHub/Pensions-Website-Design/"
path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/"


df<-read.csv(paste0(path_datos,"dataset_con_columnas.csv"))

###############################
##### Data Management #########
###############################

df$Profile_Video<-ifelse(df$sitio=="VideoPerfil", "Profile_Video", "Other")

df$t_perfil <- factor(df$t_perfil, levels = c("producto", "perfil"))



#####################
### Balance test
####################


multinom_model1 <- multinom(Treatment ~ Gender , data = df)
stargazer(multinom_model1)

names(df)


table(df$treatment)
table(df$video)


####################
### Data Analysis
#####################

#' @details Regresiones 
#' Regresión Lineal en sus diferentes combinaciones
#' 
#' #' Regresión Lineal Dependiente = Diferencia entre valence WIX y WebA. 
#' Independiente = Treatment
#' 
lm1<-lm(Valence2 ~ t_perfil + t_video + Profile_Video , data = df)

lm2<- lm(Diff2_Valence ~ t_perfil + t_video + Profile_Video , data = df)

lm3<-lm(Arousal2 ~ t_perfil + t_video + Profile_Video , data = df)

lm4<- lm(Diff2_Arousal ~ t_perfil + t_video + Profile_Video , data = df)

lm5<- lm(Diff2_Arousal ~ sitio , data = df)


stargazer(lm1, lm2, lm3, lm4, lm5)

stargazer(lm1, lm3, out=paste0(path_github,"Emotions/Outputs/arousal.tex"), type="latex",
          covariate.labels = c("Profile", "Video", "Video and Profile","Constant"), 
          dep.var.labels = c("Valence","Arousal"),  keep.stat=c("n", "rsq", "f"),
          dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
          label="tbl:arousal",
          title = " Linear regressions on the level of valence and arousal experienced by participants during the website navigation", no.space=TRUE)

