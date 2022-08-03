path_datos <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/online/data/"


pilot_data <- sitios_complete %>%
  right_join(encuestas, by = "useridn") %>%
  filter(useridn !=0) %>%
  mutate(Age = 2022 - as.numeric(Birth),
         Treatments = factor(case_when(
           website.x == "1" ~ "Perfil",
           website.x == "2" ~ "Video",
           website.x ==  "3" ~ "VideoPerfil",
           website.x == "4" ~ "Perfil",
           website.x == "5" ~ "Video",
           website.x == "6" ~ "VideoPerfil",
           website.x == "7" ~ "Baseline",
           website.x == "8" ~ "Baseline"), levels = c("Baseline", "Perfil", "Video", "VideoPerfil")),
         OptOut = case_when(
           terminaron == "si" & contesta != "C" ~ "In",
           TRUE ~ "Out"
         ),
         Pension_Type = factor(case_when(
           website.x == "1" ~ "Public",
           website.x == "2" ~ "Public",
           website.x == "3" ~ "Public",
           website.x == "8" ~ "Public",
           website.x == "4" ~ "Private",
           website.x == "5" ~ "Private",
           website.x == "6" ~ "Private",
           website.x == "7" ~ "Private"), levels = c("Public", "Private"))) %>% 
  arrange(useridn)

# breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# # labels for the breaks
# labels <- c("Night", "Morning", "Afternoon", "Evening")
# 
# pilot_data$Time_of_day <- cut(x=hour(pilot_data$StartDate), breaks = breaks, labels = labels, include.lowest=TRUE)


#Overconfidence
pilot_data$Confidence<-as.numeric(pilot_data$Confidence_1)
pilot_data$overconfidence<- pilot_data$Confidence/10 - pilot_data$correct_response/7


#Change opinion about advisor
pilot_data$Change_Advisor<-ifelse(pilot_data$PAdvice=="No" & pilot_data$Advisor=="No", "Maintain No", 
                                  ifelse (pilot_data$PAdvice=="No" & pilot_data$Advisor=="Sí", "> advisor",
                                          ifelse(pilot_data$PAdvice=="Sí" & pilot_data$Advisor=="Sí", "Maintain Yes",
                                                 ifelse(pilot_data$PAdvice=="Sí" & pilot_data$Advisor=="No", "< advisor",
                                                        ifelse(pilot_data$PAdvice=="No lo ha pensado" & pilot_data$Advisor=="Sí", "> advisor", 
                                                               ifelse(pilot_data$PAdvice=="No lo ha pensado" & pilot_data$Advisor=="No", "< advisor", "Error"
                                                               ))))))




### Correct answers for Finantial Literacy Questions 

#table(pilot_data$QMath1)
pilot_data$QMath1_correct<-ifelse(pilot_data$QMath1=="Más de $125.000.000", 1, 0)
#table(pilot_data$QMath1, pilot_data$QMath1_correct)

#table(pilot_data$QMath2)
pilot_data$QMath2_correct<-ifelse(pilot_data$QMath2=="Nunca se terminaría de pagar el crédito", 1, 0)
#table(pilot_data$QMath2, pilot_data$QMath2_correct)

#pilot_data$Qmath3num<-parse_number(pilot_data$Qmath3)
pilot_data$QMath3_correct<-ifelse(pilot_data$QMath3 == 5000,1, ifelse(pilot_data$QMath3==5, 1, 0))
#table(pilot_data$QMath3, pilot_data$QMath3_correct)


tmp<-pilot_data[, c("QMath1_correct", "QMath2_correct", "QMath3_correct") ]

tmp[is.na(tmp)] <- 0
tmp$financial_lit<-rowSums(tmp)


pilot_data$financial_lit<-tmp$financial_lit
rm(tmp)


# Time preferences
df1<-grep("Q1", names(pilot_data), value=TRUE)
df2<-grep("Q2", names(pilot_data), value=TRUE)  
df<-as.factor(c(df1, df2)) 
tmp<-pilot_data[, C(df)]

tmp$timevalue<-NA
for (i in 1:nrow(tmp)){
  NonNAindex <- which(!is.na(tmp[i,]))
  last <- max(NonNAindex)
  tmp$timevalue[i]<-colnames(tmp)[last]
}

tmp$pb<-as.numeric(gsub('\\D+','',tmp$timevalue))

pilot_data$present_bias<-tmp$pb


rm(df1, df2, df, tmp)



saveRDS(pilot_data, paste0(path_datos, "pilot_data.rds"))