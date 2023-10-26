

##########################################
#### Observational análisis of learning 
#### Denise Laroze
##########################################



library(stargazer)
library(MASS)
library(broom)
library(ggpubr)
library(naniar)
library(nnet)


rm(list=ls())
path_datos_online <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Estudio Online/"
path_datos_lab <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Laboratorio/Encuestas y sitios/"

path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/"


dfo <- readRDS(paste0(path_datos_online, "online_data.rds"))
dfo.p<-dfo[dfo$Pension_Type=="Public",]

dfl <- readRDS(paste0(path_datos_lab, "lab_data.rds"))
dfl.p<-dfl[dfl$Pension_Type=="Public",]

#



prop.table(table(dfo.p$MoAFP))
prop.table(table(dfo.p$PlanJubi))
prop.table(table(dfo.p$PostuBene))
prop.table(table(dfo.p$SelectMode.PP))





names(dfo)


prop.table(table(dfo$Comp1.PP))*100
prop.table(table(dfo$Comp2.PP))*100
prop.table(table(dfo$Comp3.PP))*100
prop.table(table(dfo$Comp4.PP))*100
prop.table(table(dfo$Comp5.PP))*100
prop.table(table(dfo$Comp6.PP))*100
prop.table(table(dfo$Comp7.PP))*100


m1.1<-lm(ncomp1~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m1.2<-lm(ncomp1~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])


stargazer(m1.1, m1.2)


m2.1<-lm(ncomp2~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m2.2<-lm(ncomp2~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])


stargazer(m2.1, m2.2)

m3.1<-lm(ncomp3~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m3.2<-lm(ncomp3~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])


stargazer(m1, m2)

m4.1<-lm(ncomp4~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m4.2<-lm(ncomp4~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])


stargazer(m4.1, m4.2)

m5.1<-lm(ncomp5~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m5.2<-lm(ncomp5~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])


stargazer(m5.1, m5.2)

m6.1<-lm(ncomp6~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m6.2<-lm(ncomp6~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])


stargazer(m6.1, m6.2)

m7.1<-lm(ncomp7~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m7.2<-lm(ncomp7~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])


stargazer(m7.1, m7.2)


