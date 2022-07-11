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



### Overconfidence graph
  ggplot(pilot_data, aes(y = overconfidence, x = Treatments, fill=Treatments)) +
  geom_boxplot() +
  labs(x ="", y = "Overconfidence") + facet_grid(. ~ Gender)

  
### 
  
  names(encuestaB_Privada_online)
  
  
  
  
  table(pilot_data$QMath1)
  table(pilot_data$QMath2)
  table(pilot_data$QMath3)
  
  ### Correct answers for Finantial Literacy Questions 
  
   #table(df$Qmath1)
  pilot_data$QMath1_correct<-ifelse(pilot_data$QMath1=="Más de $125.000.000", 1, 0)
  #table(pilot_data$QMath1, pilot_data$QMath1_correct)
 
  #table(df$Qmath2)
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
  df<-as.factor(c(tmp1, tmp2)) 
  tmp<-pilot_data[, C(df)]
  
   tmp$timevalue<-NA
   for (i in 1:nrow(tmp)){
     NonNAindex <- which(!is.na(tmp[i,]))
     last <- max(NonNAindex)
     tmp$timevalue[i]<-colnames(tmp)[last]
   }

 tmp$pb<-as.numeric(gsub('\\D+','',tmp$timevalue))
 
 pilot_data$present_bias<-tmp$pb
 
 
 rm(df1, df2, df, tmp, tmp1, tmp2)
 
 
   # 
  # df$timevalue<-tmp$timevalue
  
  
  
  
  
  


