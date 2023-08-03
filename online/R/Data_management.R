######
df <- readRDS(paste0(path_datos, "merged_data.rds"))

online_data <- df %>%
  #filter(useridn >99) %>%
  mutate(Age = 2022 - as.numeric(Birth),
         Treatments = factor(case_when(
           website == "1" ~ "Perfil",
           website == "2" ~ "Video",
           website ==  "3" ~ "VideoPerfil",
           website == "4" ~ "Perfil",
           website == "5" ~ "Video",
           website == "6" ~ "VideoPerfil",
           website == "7" ~ "Baseline",
           website == "8" ~ "Baseline"), levels = c("Baseline", "Perfil", "Video", "VideoPerfil")),
         Pension_Type = factor(case_when(
           website == "1" ~ "Public",
           website == "2" ~ "Public",
           website == "3" ~ "Public",
           website == "8" ~ "Public",
           website == "4" ~ "Private",
           website == "5" ~ "Private",
           website == "6" ~ "Private",
           website == "7" ~ "Private"), levels = c("Public", "Private"))) %>% 
         #online = ifelse(fecha.x.x == "2022-06-08", 1, 0)) 
  arrange(UID)


#### Treatment categories by types of intervention, to distinguish between the effects of one treatment versus another

online_data$Profile<-ifelse(online_data$Treatments %in% c("Perfil", "VideoPerfil"), "Profile", "Product")
#table(online_data$Treatments, online_data$Profile)
#online_data$Profile <- ordered(online_data$Profile, levels = c("Profile", "Product"))
#levels(online_data$Profile)
online_data$Profile<-as.factor(online_data$Profile)
online_data<- within(online_data, Profile <- relevel(Profile, ref = "Product"))

online_data$Video<-ifelse(online_data$Treatments %in% c("Video", "VideoPerfil"), "Video", "Text")
table(online_data$Treatments, online_data$Video)

online_data$Profile_Video<-ifelse(online_data$Treatments=="VideoPerfil", "Profile_Video", "Other")
#table(online_data$Treatments, online_data$Profile_Video)



##### Attrition measures

df$effort<-ifelse(df$encuesta=="B_Privada" | df$encuesta=="B_Publica", "Comp_questions", "No_effort")


##### Other control variables of interest
#Change opinion about advisor
online_data$Change_Advisor<-ifelse(online_data$PAdvice=="No" & online_data$Advisor=="No", "Maintain No", 
                                  ifelse (online_data$PAdvice=="No" & online_data$Advisor=="Sí", "> advisor",
                                          ifelse(online_data$PAdvice=="Sí" & online_data$Advisor=="Sí", "Maintain Yes",
                                                 ifelse(online_data$PAdvice=="Sí" & online_data$Advisor=="No", "< advisor",
                                                        ifelse (online_data$PAdvice=="Sí" & online_data$Advisor=="Sí", "Maintain",
                                                                ifelse(online_data$PAdvice=="No lo ha pensado" & online_data$Advisor=="Sí", "> advisor", 
                                                                       ifelse(online_data$PAdvice=="No lo ha pensado" & online_data$Advisor=="No", "< advisor", "Error"
                                                                       )))))))





# Time preferences
df1<-grep("Q1", names(online_data), value=TRUE)
df2<-grep("Q2", names(online_data), value=TRUE)  
df<-as.factor(c(df1, df2)) 
tmp<-online_data[, C(df)]

tmp$timevalue<-NA
for (i in 1:nrow(tmp)){
  NonNAindex <- which(!is.na(tmp[i,]))
  last <- max(NonNAindex)
  tmp$timevalue[i]<-colnames(tmp)[last]
}

tmp$pb<-as.numeric(gsub('\\D+','',tmp$timevalue))

online_data$present_bias<-tmp$pb
online_data$pb_d<-ifelse(online_data$present_bias>19999, "alto", "bajo")


#Overconfidence
online_data$Confidence<-as.numeric(online_data$Confidence_1)
online_data$overconfidence<- (online_data$Confidence/10) / (online_data$correct_response/7)

#View(online_data[, c("correct_response", "Confidence", "overconfidence")])



### Financial Literacy
### Correct answers for Financial Literacy Questions 

#table(online_data$QMath1)
online_data$QMath1b_correct<-ifelse(online_data$QMath1=="Más de $125.000.000",  1, 
                           ifelse(is.na(online_data$QMath1), NA, 0))
#table(online_data$QMath1, online_data$QMath1_correct)

#table(online_data$QMath2)
online_data$QMath2b_correct<-ifelse(online_data$QMath2=="Nunca se terminaría de pagar el crédito", 1, 
                           ifelse(is.na(online_data$QMath1), NA, 0))
#table(online_data$QMath2, online_data$QMath2_correct)

tmp<-online_data[, c("QMath1b_correct", "QMath2b_correct") ]

tmp$financial_lit_b<-rowSums(tmp)


online_data$financial_lit_b<-tmp$financial_lit_b
rm(tmp)

# Income level proxy though health care provider
table(online_data$HSist)
online_data$private_health<-ifelse( online_data$HSist == "ISAPRE" | online_data$HSist == "FF.AA. y el Orden", "Private healthcare", "Public Health or other")

online_data$private_health <-factor(online_data$private_health, levels = c("Public Health or other", "Private healthcare"))

table(online_data$HSist, online_data$private_health)


#### Education -- there are only 7 observations of the "Educación Básica" category and generated outliers 
online_data$educ_eng<-ifelse(online_data$Educ=="Educación Superior Técnico o Universitaria", "University degree", ifelse(online_data$Educ=="Postgrado", "Post-graduate degree", "Primary or high-school degree"))


rm(df1, df2, df)


saveRDS(online_data, paste0(path_datos, "online_data.rds"))

save(online_data, file=paste0(path_datos, "online_data.Rdata"))

