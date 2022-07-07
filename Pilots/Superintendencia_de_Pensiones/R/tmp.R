################################
#### Ideas to test - not clean
#### author: Denise Laroze
################################


#path_datos <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/"
path_datos <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Piloto/Superintendencia de Pensiones/online/"
# If you don´t use Rprojects functionality setwd
#setwd("C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/")
#path_github <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/Pilots/Superintendencia_de_Pensiones"

path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/"



pilot_data <- readRDS(paste0(path_datos, "pilot_data.rds"))
sitios_sm <- readRDS(paste0(path_datos, "sitios_complete.rds"))


source(paste0(path_github,"Pilots/Superintendencia_de_Pensiones/R/paquetes.R"))
#source(paste0(path_github,"Pilots/Superintendencia_de_Pensiones/R/funciones.R"))


pilot_data$Confidence<-as.numeric(pilot_data$Confidence_1)

### Hay un problema en NA que ingresan arbitrariamente a Confianza

  
  
plot(pilot_data$overconfidence)
  

#### Exceso de Confianza por tratamientos
pilot_data$overconfidence<- pilot_data$Confidence/10 - pilot_data$correct_response/7

  ggplot(pilot_data, aes(y = overconfidence, x = Treatments, fill=Treatments)) +
  geom_boxplot() +
  labs(x ="", y = "Overconfidence") + facet_grid(. ~ Gender)

  
  
  
  #Checking overconfidence
  
  table(pilot_data$Confidence)
  table(pilot_data$Confidence/10, pilot_data$correct_response/7)
  
  tmp<-pilot_data[, c("Treatments", "correct_response", "Confidence_1", "overconfidence", "website", "InfoAbruma2_1")]
  
  View(tmp)
  
  
  
  
  
  
#### Cambios de opinión en Advisor
table(pilot_data$PAdvice)  
table(pilot_data$Advisor)

#Recodificación de variable
pilot_data$Change_Advisor<-ifelse(pilot_data$PAdvice=="No" & pilot_data$Advisor=="No", "Maintain No", 
                                  ifelse (pilot_data$PAdvice=="No" & pilot_data$Advisor=="Sí", "> advisor",
                                          ifelse(pilot_data$PAdvice=="Sí" & pilot_data$Advisor=="Sí", "Maintain Yes",
                                                        ifelse(pilot_data$PAdvice=="Sí" & pilot_data$Advisor=="No", "< advisor",
                                                               ifelse (pilot_data$PAdvice=="Sí" & pilot_data$Advisor=="Sí", "Maintain",
                                                                       ifelse(pilot_data$PAdvice=="No lo ha pensado" & pilot_data$Advisor=="Sí", "> advisor", 
                                                                              ifelse(pilot_data$PAdvice=="No lo ha pensado" & pilot_data$Advisor=="No", "< advisor", "Error"
                                                                              )))))))


  
  
#tmp<-pilot_data[, c("Treatments", "PAdvice", "Advisor", "Change_Advisor")]
#View(tmp)
  
table(pilot_data$Treatments, pilot_data$Change_Advisor)
  
  
  
  
  
  
  
  
  



