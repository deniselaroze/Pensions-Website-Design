#########################################
##### Sitios web pensiones Data Management
##### code: Denise Laroze
##### 2022
#########################################



library(naniar)
library(readr)
library(matrixStats)
library(BBmisc)
library(dplyr)
rm(list=ls())

setwd("C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Piloto/Superintendencia de Pensiones/online")

df1<-(read_csv("Encuesta_A.csv")[-1:-2,])
df2<-(read_csv("Encuesta_B_Publica.csv")[-1:-2,])
df3<-(read_csv("Encuesta_B_Privada.csv")[-1:-2,])
df4<-(read_csv("Encuesta_C_Privada.csv")[-1:-2,])
df5<-(read_csv("Encuesta_C_Publica.csv")[-1:-2,])




################# Clean #######################################333
drop<-c(    "StartDate", "EndDate", "Status", "IPAddress", "Progress","Duration (in seconds)", "Finished", "RecordedDate", 
            "ResponseId", "RecipientLastName", "RecipientFirstName",  "RecipientEmail", "ExternalReference", 
            "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage")

df1<-df1[,!(names(df1) %in% drop)]
df2<-df2[,!(names(df2) %in% drop)]
df3<-df3[,!(names(df3) %in% drop)]
df4<-df4[,!(names(df4) %in% drop)]
df5<-df5[,!(names(df5) %in% drop)]



### Correct responses 

#Public Pensions
#library(tidyverse)

comp<- df2 %>% select(contains("Comp"))

comp$ncomp1<-ifelse(df2$Comp1.PP=="A partir de los 65 años", 1, 0)
comp$ncomp2<-ifelse(df2$Comp2.PP=="Tener ClaveÚnica", 1, 0)
comp$ncomp3<-ifelse(df2$Comp3.PP=="Sí", 1, 0)
comp$ncomp4<-ifelse(df2$Comp4.PP=="A través del Estado con los impuestos", 1, 0)
comp$ncomp5<-ifelse(df2$Comp5.PP=="Por encontrarse dentro del 9% de mayores ingresos", 1, 0)
comp$ncomp6<-ifelse(df2$Comp6.PP=="No", 1, 0)
comp$ncomp7<-ifelse(df2$Comp7.PP=="$1.000.000", 1, 0)

ncomp<- comp %>% select(contains("ncomp"))
comp$correct_response <-rowSums(ncomp, na.rm = T)
df2$correct_response<-comp$correct_response
rm(comp, ncomp)


### Pensiones privadas

comp<- df3 %>% select(contains("Comp"))

comp$ncomp1<-ifelse(df3$Comp1.RP=="No", 1, 0)
comp$ncomp2<-ifelse(df3$Comp2.RP=="Solicitar el Certificado de Saldo en la AFP", 1, 0)
comp$ncomp3<-ifelse(df3$Comp3.RP=="No", 1, 0)
comp$ncomp4<-ifelse(df3$Comp4.RP=="Para comparar todas las ofertas de pensión disponibles en un sólo lugar", 1, 0)
comp$ncomp5<-ifelse(df3$Comp5.RP=="Sí", 1, 0)
comp$ncomp6<-ifelse(df3$Comp6.RP=="Es voluntario", 1, 0)
comp$ncomp7<-ifelse(df3$Comp7.RP=="El/la afiliado/a", 1, 0)


comp$ncomp8<-ifelse(df3$Comp1.RV=="No", 1, 0)
comp$ncomp9<-ifelse(df3$Comp2.RV=="Solicitar el Certificado de Saldo en la AFP", 1, 0)
comp$ncomp10<-ifelse(df3$Comp3.RV=="No", 1, 0)
comp$ncomp11<-ifelse(df3$Comp4.RV=="Para comparar todas las ofertas de pensión disponibles en un sólo lugar", 1, 0)
comp$ncomp12<-ifelse(df3$Comp5.RV=="Sí", 1, 0)
comp$ncomp13<-ifelse(df3$Comp6.RV=="Es voluntario", 1, 0)
comp$ncomp14<-ifelse(df3$Comp7.RV=="La Compañía de Seguros", 1, 0) 


comp$ncomp15<-ifelse(df3$Comp1.MM=="No", 1, 0)
comp$ncomp16<-ifelse(df3$Comp2.MM=="Solicitar el Certificado de Saldo en la AFP", 1, 0)
comp$ncomp17<-ifelse(df3$Comp3.MM=="No", 1, 0)
comp$ncomp18<-ifelse(df3$Comp4.MM=="Para comparar todas las ofertas de pensión disponibles en un sólo lugar", 1, 0)
comp$ncomp19<-ifelse(df3$Comp5.MM=="Sí", 1, 0)
comp$ncomp20<-ifelse(df3$Comp6.MM=="Es voluntario", 1, 0)
comp$ncomp21<-ifelse(df3$Comp7.MM=="Ambos: la Compañía de Seguros y el/la afiliado/a", 1, 0) 


ncomp<- comp %>% select(contains("ncomp"))
comp$correct_response <-rowSums(ncomp,na.rm = T)
df3$correct_response<-comp$correct_response
rm(comp, ncomp)



######## Merge datasets

#DV

dvars<-c("uemail", "UID" ,"InfoUtil_1", "InfoAbruma2_1", "Curiosity_1", "Confidence_1" , "total_reward", "correct_response" )

dv2<-df2[,(names(df2) %in% dvars)]
dv3<-df3[,(names(df3) %in% dvars)]

dv<-rbind(dv2, dv3)


df<-merge(df1, dv, by.x = "useridn", by.y = "uemail", all = T)



#df<-merge(df, df4, by.x = "useridn", by.y = "uemail", all = T)
#df<-merge(df, df5, by.x = "useridn", by.y = "uemail", all = T)

### Eliminate non consent and people that are retired 

df<-df[complete.cases(df$QRead),]
df<-df[df$QRead!="No", ] # Does not give Consent
df<-df[df$Tram_jubilacion=="No", ] # indicates he/she is retired
df<-df[df$Tram_jubilacion=="No", ]

View(subset(df, select = c("EPension_PuvsPri", "total_reward", "correct_response" )))



### Recode

web<-df$website
df$treat<-recode(web, 
            "1" = "Perfil",
            "2" = "Video",
            "3" = "VideoPerfil",
            "4" = "Perfil",
            "5" = "Video",
            "6" = "VideoPerfil",
            "7" = "LinaBase",
            "8" = "LinaBase"
)


## Define as numeric
df$total_reward<-as.numeric(df$total_reward)
df$InfoAbruma2_1<-as.numeric(df$InfoAbruma2_1)
df$InfoUtil_1<-as.numeric(df$InfoUtil_1)
df$Curiosity_1<-as.numeric(df$Curiosity_1)
df$Confidence_1<-as.numeric(df$Confidence_1)


## If they reached the second survey

df$second_survey<-ifelse(is.na(df$InfoUtil_1), 1, 0)
df$second_survey_end<-ifelse(is.na(df$InfoUtil_1), 1, 0)



###########################################
save(df, file = "Pilot_data.Rdata")

write.table(df, "Pilot_data.csv", sep=",", row.names = F)
















#### Eliminate duplicated observations/participations
#df$rut<-gsub('-|\\.| |.{1}$','',df$Qrut)
#df$rut<-as.numeric(df$rut) 
#View(df[is.na(df$rut),c("Qrut", "rut", "treatment1", "Qt1")]) To check if warnings are irrelevant
#df<-df[!duplicated("rut"),] ## eliminate duplicated ruts ### Total observations, before cleaning. 


# Eliminating invalid observations, protocol violations
#df<-df[df$rut<10500000,] ## people that are too young.


# Eliminate ruts with doubtful origin
#ndf$rut_elim2<-gsub('-|\\.| |.{1}$','',ndf$rut_elim)
#elim<-as.numeric(ndf$rut_elim2)

#df<-df[!df$rut %in% elim,]


#### Identify time preference variable
# tmp<-df[, c(paste0("Q", 207:237), "Q239")]
# 
# tmp$timevalue<-NA
# for (i in 1:nrow(tmp)){
#   NonNAindex <- which(!is.na(tmp[i,]))
#   last <- max(NonNAindex)
#   tmp$timevalue[i]<-colnames(tmp)[last]
# }
# 
# df$timevalue<-tmp$timevalue








###################
### Recode  - OLD needs to be updated
######################
table(df$Qmath5)

### Math 5 Question, price of ball
df$Qmath5num<-parse_number(df$Qmath5)

#View(df[,c("Qmath5", "Qmath5num")])
#table(df$Qmath5num)
df$Qmath5_correct<-ifelse(df$Qmath5num == 5000,1, ifelse(df$Qmath5num==5, 1, 0))
#table(df$Qmath5num, df$Qmath5_correct)

### correct answers  for Qmath3 and Qmath4

#table(df$Qmath3)
df$Qmath3_correct<-ifelse(df$Qmath3=="M?s de $125.000.000", 1, 0)
#table(df$Qmath3, df$Qmath3_correct)


#table(df$Qmath4)
df$Qmath4_correct<-ifelse(df$Qmath4=="Nunca se terminar?a de pagar el cr?dito", 1, 0)
#table(df$Qmath4, df$Qmath4_correct)

tmp<-df[, c("Qmath3_correct", "Qmath4_correct", "Qmath5_correct") ]
tmp[is.na(tmp)] <- 0
tmp$financial_lit<-rowSums(tmp)

df$financial_lit<-tmp$financial_lit
rm(tmp)



###########################################
### Anonimise -- Pilot Data is annonimous 
###########################################

# eliminate

#elim<-c( "Qtipeaccount", "Qnamebank", "Qnaccount1", "Qnaccount2" ,
#         "Duration..in.seconds.", "ResponseId", "LocationLatitude", "LocationLongitude", "DistributionChannel",   "UserLanguage",         
#          "Q250_First.Click",   "Q250_Last.Click" ,  "Q250_Page.Submit", "Q250_Click.Count" , "Q255_Browser", "Q255_Version",         
#          "Q255_Operating.System", "Q255_Resolution", "Qrut", "Qtipeaccount" , "Qnamebank", "Qnaccount1", "Qnaccount2"  ,"rut")


#df <- df[ , !(names(df) %in% elim)]





