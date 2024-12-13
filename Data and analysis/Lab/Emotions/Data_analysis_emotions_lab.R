
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
#path_github <- "C:/Users/DCCS2/Documents/GitHub/Pensions-Website-Design/Data and analysis/Lab/"
path_datos<-"C:/Users/DCCS2/Dropbox/R&R Chilean pensions paper/"

#path_datos <- "C:/Users/DCCS2/Dropbox/Sitios web/Datos Laboratorio/Videos/"
path_github <- "C:/Users/DCCS2/Documents/GitHub/Pensions-Website-Design/"


df<-read.csv2(paste0(path_datos,"dataset_con_columnas.csv"))


#View(df)


###############################
##### Data Management #########
###############################

df$Profile_Video<-ifelse(df$sitio=="VideoPerfil", "Profile_Video", "Other")

df$t_perfil <- factor(df$t_perfil, levels = c("producto", "perfil"))

#####################
### Balance 
####################

table(df$t_perfil, df$video)
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

stargazer(lm1, lm3, out=paste0(path_github,"Emotions/Outputs/Table_7.tex"), type="latex",
          covariate.labels = c("Profile$\\_i$", "Video$\\_j$", "Profile$\\_i$xVideo$\\_j$","Constant"), 
          dep.var.labels = c("Valence","Arousal"),  keep.stat=c("n", "rsq", "f"),
          dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
          label="tbl:arousal",
          title = " Linear regressions on the level of valence and arousal experienced by participants during the website navigation", no.space=TRUE)


lm1<-lm(ValenceX2 ~ t_perfil + t_video + Profile_Video , data = df)

lm2<-lm(ArousalX2 ~ t_perfil + t_video + Profile_Video , data = df)

lm3<-lm(Happy2 ~ t_perfil + t_video + Profile_Video , data = df)

lm4<-lm(Sad2 ~ t_perfil + t_video + Profile_Video , data = df)
lm5<-lm(Angry2 ~ t_perfil + t_video + Profile_Video , data = df)

lm6<-lm(Surprised2 ~ t_perfil + t_video + Profile_Video , data = df)

lm7<-lm(Scared2 ~ t_perfil + t_video + Profile_Video , data = df)

lm8<-lm(Disgusted2 ~ t_perfil + t_video + Profile_Video , data = df)

#stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8)

stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, out=paste0(path_datos,"emotions.html"), type="html")



#covariate.labels = c("Profile$\\_i$", "Video$\\_j$", "Profile$\\_i$xVideo$\\_j$","Constant"), 
#dep.var.labels = c("Valence","Arousal"),  keep.stat=c("n", "rsq", "f"),
#dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
#label="tbl:arousal",
#title = " Linear regressions on the level of valence and arousal experienced by participants during the website navigation", no.space=TRUE)









