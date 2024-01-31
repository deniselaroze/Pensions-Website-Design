

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

# Pregunta ¿A qué edad puede solicitar los beneficios de vejez?
prop.table(table(dfo$Comp1.PP))*100
prop.table(table(dfl$Comp1.PP))*100


# Pregunta ¿Qué se necesita para realizar la solicitud de beneficios en línea?
prop.table(table(dfo$Comp2.PP))*100
prop.table(table(dfl$Comp2.PP))*100

# ¿Puede un/a extranjero/a acceder a los beneficios de pensiones? 
prop.table(table(dfo$Comp3.PP))*100
prop.table(table(dfl$Comp3.PP))*100

# ¿Cómo se financia la Pensión Garantizada Universal?
prop.table(table(dfo$Comp4.PP))*100
prop.table(table(dfl$Comp4.PP))*100

# ¿Por qué motivo a alguien le podrían rechazar el acceso a los beneficios?
prop.table(table(dfo$Comp5.PP))*100
prop.table(table(dfl$Comp5.PP))*100

# ¿Es necesario estar jubilado/a para acceder a la Pensión Garantizada Universal?
prop.table(table(dfo$Comp6.PP))*100
prop.table(table(dfl$Comp6.PP))*100

# ¿En qué monto fue fijada la Pensión Superior en 2022?
prop.table(table(dfo$Comp7.PP))*100
prop.table(table(dfl$Comp7.PP))*100





# Pregunta ¿A qué edad puede solicitar los beneficios de vejez?
m1.1<-lm(ncomp1~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m1.2<-lm(ncomp1~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])
m1.3<-lm(ncomp1~ Profile + Video + Profile_Video, 
         data = dfl[dfl$Pension_Type=="Public",])
m1.4<-lm(ncomp1~ Treatments, 
         data = dfl[dfl$Pension_Type=="Public",])

stargazer(m1.1, m1.2, m1.3, m1.4)


# Pregunta ¿Qué se necesita para realizar la solicitud de beneficios en línea?
m2.1<-lm(ncomp2~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m2.2<-lm(ncomp2~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])
m2.3<-lm(ncomp2~ Profile + Video + Profile_Video, 
         data = dfl[dfl$Pension_Type=="Public",])
m2.4<-lm(ncomp2~ Treatments, 
         data = dfl[dfl$Pension_Type=="Public",])

stargazer(m2.1, m2.2, m2.3, m2.4)


# ¿Puede un/a extranjero/a acceder a los beneficios de pensiones? 
m3.1<-lm(ncomp3~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m3.2<-lm(ncomp3~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])
m3.3<-lm(ncomp3~ Profile + Video + Profile_Video, 
         data = dfl[dfl$Pension_Type=="Public",])
m3.4<-lm(ncomp3~ Treatments, 
         data = dfl[dfl$Pension_Type=="Public",])

stargazer(m3.1, m3.2, m3.3, m3.4)


# ¿Cómo se financia la Pensión Garantizada Universal?
m4.1<-lm(ncomp4~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m4.2<-lm(ncomp4~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])

m4.3<-lm(ncomp4~ Profile + Video + Profile_Video, 
         data = dfl[dfl$Pension_Type=="Public",])
m4.4<-lm(ncomp4~ Treatments, 
         data = dfl[dfl$Pension_Type=="Public",])

stargazer(m4.1, m4.2, m4.3, m4.4)


# ¿Por qué motivo a alguien le podrían rechazar el acceso a los beneficios?
m5.1<-lm(ncomp5~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m5.2<-lm(ncomp5~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])

m5.3<-lm(ncomp5~ Profile + Video + Profile_Video, 
         data = dfl[dfl$Pension_Type=="Public",])
m5.4<-lm(ncomp5~ Treatments, 
         data = dfl[dfl$Pension_Type=="Public",])

stargazer(m5.1, m5.2, m5.3, m5.4)


# ¿Es necesario estar jubilado/a para acceder a la Pensión Garantizada Universal?
m6.1<-lm(ncomp6~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m6.2<-lm(ncomp6~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])
m6.3<-lm(ncomp6~ Profile + Video + Profile_Video, 
         data = dfl[dfl$Pension_Type=="Public",])
m6.4<-lm(ncomp6~ Treatments, 
         data = dfl[dfl$Pension_Type=="Public",])

stargazer(m6.1, m6.2, m6.3, m6.4)



# ¿En qué monto fue fijada la Pensión Superior en 2022?
m7.1<-lm(ncomp7~ Profile + Video + Profile_Video, 
       data = dfo[dfo$Pension_Type=="Public",])
m7.2<-lm(ncomp7~ Treatments, 
       data = dfo[dfo$Pension_Type=="Public",])
m7.3<-lm(ncomp7~ Profile + Video + Profile_Video, 
         data = dfl[dfl$Pension_Type=="Public",])
m7.4<-lm(ncomp7~ Treatments, 
         data = dfl[dfl$Pension_Type=="Public",])


stargazer(m7.1, m7.2, m7.3, m7.4)



####
### Comprensión derechos extranjeros


ex<-lm(ncomp1 ~ Age + Gender + private_health  + as.factor(financial_lit_b), data=dfo.p)
summary(ex)

ex2<-lm(ncomp1 ~ Age + Gender + as.factor(financial_lit_b)+ pb_d, data=dfo.p)

ex3<-lm(ncomp1 ~ Age + Gender  + as.factor(financial_lit_b)+ pb_d, data=dfl.p)

stargazer(ex2, ex3)




