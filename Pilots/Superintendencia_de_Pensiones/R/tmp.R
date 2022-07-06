
#### Ideas to test - not clean


pilot_data$Treatments
pilot_data$correct_response



pilot_data$Confidence<-as.numeric(pilot_data$Confidence_1)
pilot_data$overconfidence<- pilot_data$Confidence/10 - pilot_data$correct_response/7


table(pilot_data$Confidence)
table(pilot_data$Confidence, pilot_data$correct_response/7)

pilot_data$InfoAbruma2_1


tmp<-pilot_data[, c("Treatments", "correct_response", "Confidence_1", "overconfidence", "website", "InfoAbruma2_1")]

View(tmp)


