data <- sitios_complete %>%
  right_join(encuestas, by = "useridn") %>%
  filter(useridn >99) %>%
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
  arrange(useridn)

data$OptOut = ifelse(data$terminaron == "si" & data$contesta == "B" , "In", "Out")

#breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
#labels <- c("Night", "Morning", "Afternoon", "Evening")

#data$Time_of_day <- cut(x=hour(data$StartDate), breaks = breaks, labels = labels, include.lowest=TRUE)



#Overconfidence
data$Confidence<-as.numeric(data$Confidence_1)
data$overconfidence<- data$Confidence/10 - data$correct_response/7


#Change opinion about advisor
data$Change_Advisor<-ifelse(data$PAdvice=="No" & data$Advisor=="No", "Maintain No", 
                                  ifelse (data$PAdvice=="No" & data$Advisor=="Sí", "> advisor",
                                          ifelse(data$PAdvice=="Sí" & data$Advisor=="Sí", "Maintain Yes",
                                                 ifelse(data$PAdvice=="Sí" & data$Advisor=="No", "< advisor",
                                                        ifelse (data$PAdvice=="Sí" & data$Advisor=="Sí", "Maintain",
                                                                ifelse(data$PAdvice=="No lo ha pensado" & data$Advisor=="Sí", "> advisor", 
                                                                       ifelse(data$PAdvice=="No lo ha pensado" & data$Advisor=="No", "< advisor", "Error"
                                                                       )))))))




### Correct answers for Financial Literacy Questions 

#table(data$QMath1)
data$QMath1_correct<-ifelse(data$QMath1=="Más de $125.000.000", 1, 
                                  ifelse(is.na(data$QMath1), NA, 0))
#table(data$QMath1, data$QMath1_correct)

#table(data$QMath2)
data$QMath2_correct<-ifelse(data$QMath2=="Nunca se terminaría de pagar el crédito", 1, 
                                  ifelse(is.na(data$QMath1), NA, 0))
#table(data$QMath2, data$QMath2_correct)

#data$Qmath3num<-parse_number(data$Qmath3)
data$QMath3_correct<-ifelse(data$QMath3 == 5000,1, 
                                  ifelse(data$QMath3==5, 1,
                                         ifelse(is.na(data$QMath1), NA, 0)))
#table(data$QMath3, data$QMath3_correct)


tmp<-data[, c("QMath1_correct", "QMath2_correct", "QMath3_correct") ]

#tmp[is.na(tmp)] <- 0
tmp$financial_lit<-rowSums(tmp)


data$financial_lit<-tmp$financial_lit
rm(tmp)


# Time preferences
df1<-grep("Q1", names(data), value=TRUE)
df2<-grep("Q2", names(data), value=TRUE)  
df<-as.factor(c(df1, df2)) 
tmp<-data[, C(df)]

tmp$timevalue<-NA
for (i in 1:nrow(tmp)){
  NonNAindex <- which(!is.na(tmp[i,]))
  last <- max(NonNAindex)
  tmp$timevalue[i]<-colnames(tmp)[last]
}

tmp$pb<-as.numeric(gsub('\\D+','',tmp$timevalue))

data$present_bias<-tmp$pb
data$pb_d<-ifelse(data$present_bias>19999, "alto", "bajo")


rm(df1, df2, df, tmp)


saveRDS(data, paste0(path_datos, "lab_data.rds"))

save(data, file=paste0(path_datos, "lab_data.Rdata"))

