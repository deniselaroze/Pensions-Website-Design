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

setwd("C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Piloto/Superintendencia de Pensiones")

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



######## Merge datasets

#DV

dvars<-c("uemail", "UID" ,"InfoUtil_1", "InfoAbruma2_1", "Curiosity_1", "Confidence_1" , "total_reward" )

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

View(subset(df, website="7", select = c("EPension_PuvsPri", "total_reward" )))

View(df[df$website==7,])

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





