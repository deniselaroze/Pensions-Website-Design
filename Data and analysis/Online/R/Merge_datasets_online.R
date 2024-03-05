
##############################
#### Data cleaning first stage of the data collection
##############################

encuesta_A <- "Encuesta_A_anonimo.csv"

encuestaA_online <- read_csv(paste0(path_datos, encuesta_A))[-1,] %>% 
  mutate(#fecha = lubridate::as_date(EndDate, format = "%Y-%m-%d"),
         sitioderivado = case_when(
         nchar(useridn) == 4  ~ stringi::stri_sub(UID, 8, 8),
         nchar(useridn) == 3  ~ stringi::stri_sub(UID, 7, 7)),
         useridn = as.numeric(useridn)) %>%
  filter(QRead=="Sí, he leído y entendido las reglas. Chile, 2022") %>% # 2 people drop out because they don't give consent
  filter(Tram_jubilacion=="No") %>% # 292 people are excluded because they are already retired, 1 person drops out early 
  filter(is.na(EpistemicC_1)==FALSE) %>% # 38 people are excluded because they declare their age to be under 50
  filter(is.na(UID)==FALSE) %>% ## 1 person leaves in the instructions to the experimental section 
#  filter(is.na(EPension_PuvsPri)==FALSE) %>%
#  group_by(useridn) %>% 5
 # filter(Progress==max(Progress)) %>%
  ungroup() %>%
  select(-Status,
         -RecordedDate,
         -ResponseId,
         -ExternalReference,
         -UserLanguage) %>% distinct()
encuestaA_online <- encuestaA_online[,colSums(is.na(encuestaA_online))<nrow(encuestaA_online)]


saveRDS(encuestaA_online, paste0(path_datos, "encuesta_A_clean.rds"))

##################################################
#### State two - Website Navigation: Process Data 
##################################################


sitios <-
  list.files(path = paste0(path_datos, "clicks"),
             pattern = "Clicks", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  unite("inicio_wix", c("inicio_wix.p1", "inicio_wix.p8"), na.rm = TRUE, remove = TRUE)

# remove columns with complete NA
sitios <- sitios[,colSums(is.na(sitios))<nrow(sitios)]

# remove rows with complete NA
N = ncol(sitios) - 4 # check the number of columns that will always have data (4 in these case)
sitios <- delete.na(sitios, N)

# Transform and create variables
sitios2 <- sitios %>%
  mutate(useridn = as.numeric(gsub("-", "" , substr(userid, 1,4))),
         sitio= stringi::stri_sub(userid, -1),
         fecha = lubridate::as_date(`Created date`, format = "%Y-%m-%d"),
         created_date = lubridate::as_datetime(`Created date`, "%Y-%m-%dT%H:%M:%SZ"),
         updated_date = lubridate::as_datetime(`Updated date`, "%Y-%m-%dT%H:%M:%SZ")) %>%
  mutate(
    tiempo_sec_1click_nclick = as.numeric(difftime(updated_date, created_date, units="secs")),
    tiempo_min_1click_nclick = as.numeric(difftime(updated_date, created_date, units="secs"))/60)  %>%
  filter(useridn != 0) %>%
  group_by(useridn) %>% 
  filter(tiempo_sec_1click_nclick==max(tiempo_sec_1click_nclick)) %>%
  ungroup() %>%
  mutate(na_count = apply(., 1, function(x) sum(is.na(x)))) %>%
  mutate(n_clicks = dim(.)[2]-na_count-11)

# intermediate data for answer variable

sitios2$dummy_web<-"Yes web"

saveRDS(sitios2, paste0(path_datos,"sitios_complete.rds")) ## using the largest dataet without missing values



###############################################################################
# Merge and transformation of the third stage of data collection - post website
###############################################################################
# Encuesta B Privada, load and transformation

encuesta_B_Pri <- "Encuesta_B_Privada.csv"

B_Privada <- read_csv(paste0(path_datos, encuesta_B_Pri)) %>% 
  mutate(fecha = lubridate::as_date(EndDate, format = "%Y-%m-%d")) %>%
  mutate(EndDate = lubridate::ymd_hms(EndDate),
         StartDate = lubridate::ymd_hms(StartDate),
         encuesta = "B_Privada",
         uemail = as.numeric(uemail),
         total_reward = as.numeric(total_reward)) %>%
  #dplyr::filter(fecha  == "2022-05-20" | fecha  == "2022-05-27" | fecha == "2022-06-08" | fecha == "2022-06-07") %>%
  filter(is.na(uemail)==FALSE) %>%
#  group_by(uemail) %>% #### selecting the option with the most progress
#  filter(Progress==max(Progress)) %>%
#  ungroup() %>%
#  group_by(uemail) %>% 
#  filter(StartDate==min(StartDate)) %>% ### and first observation of the one with the most progress
#  ungroup() %>%
#  mutate(perfilfiltrar = ifelse(is.na(perfil), "no_filtrar", 
#                                ifelse(perfil==0, "no_filtrar", "filtrar"))) %>%
#  filter(perfilfiltrar != "filtrar") %>%
  select(#-perfilfiltrar,
         EndDate,
         StartDate,
         encuesta,
         contains("Comp"),
         uemail, 
         UID,
         age,
         perfil,
         genero,
         Progress,
         Finished,
         `Duration (in seconds)`,
         Advisor,
         Recomendar,
         SelectMode.PV,
         SelectMode.under50,
         Recomendar_NPS_GROUP,
         Confused,
         InfoUtil_1, 
         Curiosity_1, 
         Confidence_1 ,
         esfuerzo,
         Facilidad,
         Dificultad,
         Dificultad_1_TEXT,
         total_reward,
         contains("Math"),
         contains("Q1"),
         contains("Q2")
         
         ) %>%
  rename(MB_Despues = SelectMode.PV)
B_Privada <- B_Privada[,colSums(is.na(B_Privada))<nrow(B_Privada)]

# Encuesta B Publica, load and transformation

encuesta_B_Pu <- "Encuesta_B_publica.csv"

B_Publica <- read_csv(paste0(path_datos, encuesta_B_Pu)) %>% 
  mutate(fecha = lubridate::as_date(EndDate, format = "%Y-%m-%d")) %>%
  mutate(EndDate = lubridate::ymd_hms(EndDate),
         StartDate = lubridate::ymd_hms(StartDate),
         encuesta = "B_Publica",
         uemail = as.numeric(uemail),
         total_reward = as.numeric(total_reward))  %>%
  #dplyr::filter(fecha  == "2022-05-20" | fecha  == "2022-05-27" | fecha == "2022-06-08" | fecha == "2022-06-07") %>%
  filter(is.na(uemail)==FALSE) %>%
#  group_by(uemail) %>% 
#  filter(Progress==max(Progress)) %>%
#  ungroup() %>%
#  group_by(uemail) %>% 
#  filter(StartDate==min(StartDate)) %>%
#  ungroup() %>%
#  mutate(perfilfiltrar = ifelse(is.na(perfil), "no_filtrar", 
#                                ifelse(perfil==0, "no_filtrar", "filtrar"))) %>%
#  filter(perfilfiltrar != "filtrar") %>%
  select(EndDate,
         StartDate,
         encuesta,
         contains("Comp"),
         uemail, 
         UID,
         age,
         perfil,
         genero,
         Progress,
         Finished,
         `Duration (in seconds)`,
         Recomendar,
         SelectMode.PP,
         SelectMode.under50,
         Recomendar_NPS_GROUP,
         Confused,
         InfoUtil_1, 
         Curiosity_1, 
         Confidence_1 ,
         esfuerzo,
         Facilidad,
         Dificultad,
         Dificultad_1_TEXT,
         total_reward,
         contains("Math"),
         contains("Q1"),
         contains("Q2"))
B_Publica <- B_Publica[,colSums(is.na(B_Publica))<nrow(B_Publica)]

# Encuesta C Privada, load and transformation

encuesta_C_Pri <- "Encuesta_C_privadas.csv"

C_Privada <- read_csv(paste0(path_datos, encuesta_C_Pri)) %>% 
  mutate(fecha = lubridate::as_date(EndDate, format = "%Y-%m-%d")) %>%
  mutate(EndDate = lubridate::ymd_hms(EndDate),
         StartDate = lubridate::ymd_hms(StartDate),
         encuesta = "C_Privada",
         uemail = as.numeric(uemail), 
         total_reward = as.numeric(9200)
         ) %>%
  #dplyr::filter(fecha  == "2022-05-20" | fecha  == "2022-05-27" | fecha == "2022-06-08" | fecha == "2022-06-07") %>%
  filter(is.na(uemail)==FALSE) %>%
#  group_by(uemail) %>% 
#  filter(Progress==max(Progress)) %>%
#  ungroup() %>%
#  group_by(uemail) %>% 
#  filter(StartDate==min(StartDate)) %>%
#  ungroup() %>%
#  mutate(perfilfiltrar = ifelse(is.na(perfil), "no_filtrar", 
#                                ifelse(perfil==0, "no_filtrar", "filtrar"))) %>%
#  filter(perfilfiltrar != "filtrar") %>%
  select(fecha,
         EndDate,
         StartDate,
         encuesta,
         contains("Comp"),
         uemail, 
         UID,
         age, 
         perfil,
         genero,
         Progress, 
         Finished,
         `Duration (in seconds)`,
         Advisor,
         Confused,
         InfoUtil_1, 
         Facilidad,
         Dificultad,
         Dificultad_1_TEXT,
         Recomendar,
         Recomendar_NPS_GROUP,
         SelectMode.PV,
         SelectMode.under50,
         total_reward,
         contains("Math"),
         contains("Q1"),
         contains("Q2")) %>%
  rename(MB_Despues = SelectMode.PV)
C_Privada <- C_Privada[,colSums(is.na(C_Privada))<nrow(C_Privada)]

# Encuesta C Publica, load and transformation

encuesta_C_Pu <- "Encuesta_C_publicas.csv"

C_Publica <- read_csv(paste0(path_datos, encuesta_C_Pu)) %>% 
  mutate(fecha = lubridate::as_date(EndDate, format = "%Y-%m-%d")) %>%
  mutate(EndDate = lubridate::ymd_hms(EndDate),
         StartDate = lubridate::ymd_hms(StartDate),
         encuesta = "C_Publica",
         uemail = as.numeric(uemail),
         total_reward = as.numeric(6500)
         ) %>%
  #dplyr::filter(fecha  == "2022-05-20" | fecha  == "2022-05-27" | fecha == "2022-06-08" | fecha == "2022-06-07") %>%
  filter(is.na(uemail)==FALSE) %>%
#  group_by(uemail) %>% 
#  filter(Progress==max(Progress)) %>%
#  ungroup() %>%
#  group_by(uemail) %>% 
#  filter(StartDate==min(StartDate)) %>%
#  ungroup() %>%
  select(fecha,
         EndDate,
         StartDate,
         encuesta,
         contains("Comp"),
         uemail, 
         UID,
         age, 
         perfil,
         genero,
         Progress, 
         Finished,
         `Duration (in seconds)`,
         Confused,
         InfoUtil_1, 
         Facilidad,
         Dificultad,
         Dificultad_1_TEXT,
         Recomendar,
         Recomendar_NPS_GROUP,
         SelectMode.PP,
         SelectMode.under50,
         PostuBene,
         total_reward,
         contains("Math"),
         contains("Q1"),
         contains("Q2")) %>%
  rename(MB_Despues = SelectMode.PP)
C_Publica <- C_Publica[,colSums(is.na(C_Publica))<nrow(C_Publica)]


# Encuesta B Privada, computo de respuestas correctas

B_Privada <- B_Privada %>%
  mutate(
    ncomp1 = ifelse(Comp1.RP=="No", 1, ifelse(is.na(Comp1.RP), NA, 0)),
    ncomp2=ifelse(Comp2.RP=="Solicitar el Certificado de Saldo en la AFP", 1, ifelse(is.na(Comp2.RP), NA, 0)),
    ncomp3=ifelse(Comp3.RP=="No", 1, ifelse(is.na(Comp3.RP), NA, 0)),
    ncomp4=ifelse(Comp4.RP=="Para comparar todas las ofertas de pensión disponibles en un sólo lugar", 1, ifelse(is.na(Comp4.RP), NA, 0)),
    ncomp5=ifelse(Comp5.RP=="Sí", 1, ifelse(is.na(Comp5.RP), NA, 0)),
    ncomp6=ifelse(Comp6.RP=="Es voluntario", 1, ifelse(is.na(Comp6.RP), NA, 0)),
    ncomp7=ifelse(Comp7.RP=="El/la afiliado/a", 1, ifelse(is.na(Comp7.RP), NA, 0)),
    ncomp8=ifelse(Comp1.RV=="No", 1, ifelse(is.na(Comp1.RV), NA, 0)),
    ncomp9=ifelse(Comp2.RV=="Solicitar el Certificado de Saldo en la AFP", 1, ifelse(is.na(Comp2.RV), NA, 0)),
    ncomp10=ifelse(Comp3.RV=="No", 1, ifelse(is.na(Comp3.RV), NA, 0)),
    ncomp11=ifelse(Comp4.RV=="Para comparar todas las ofertas de pensión disponibles en un sólo lugar", 1, ifelse(is.na(Comp4.RV), NA, 0)),
    ncomp12=ifelse(Comp5.RV=="Sí", 1, ifelse(is.na(Comp5.RV), NA, 0)),
    ncomp13=ifelse(Comp6.RV=="Es voluntario", 1, ifelse(is.na(Comp6.RV), NA, 0)),
    ncomp14=ifelse(Comp7.RV=="La Compañía de Seguros", 1, ifelse(is.na(Comp7.RV), NA, 0)),
    ncomp15=ifelse(Comp1.MM=="No", 1, ifelse(is.na(Comp1.MM), NA, 0)),
    ncomp16=ifelse(Comp2.MM=="Solicitar el Certificado de Saldo en la AFP", 1, ifelse(is.na(Comp2.MM), NA, 0)),
    ncomp17=ifelse(Comp3.MM=="No", 1, ifelse(is.na(Comp3.MM), NA, 0)),
    ncomp18=ifelse(Comp4.MM=="Para comparar todas las ofertas de pensión disponibles en un sólo lugar", 1, ifelse(is.na(Comp4.MM), NA, 0)),
    ncomp19=ifelse(Comp5.MM=="Sí", 1, ifelse(is.na(Comp5.MM), NA, 0)),
    ncomp20=ifelse(Comp6.MM=="Es voluntario", 1, ifelse(is.na(Comp6.MM), NA, 0)),
    ncomp21=ifelse(Comp7.MM=="Ambos: la Compañía de Seguros y el/la afiliado/a", 1, ifelse(is.na(Comp7.MM), NA, 0)) 
  )



ncomp<- B_Privada %>% select(contains("ncomp"))
obliga<-B_Privada[ c("ncomp1", "ncomp8", "ncomp15")]
inicio<-B_Privada[ c("ncomp2", "ncomp9", "ncomp16")]
elegir<-B_Privada[ c("ncomp3", "ncomp10", "ncomp17")]
sirve.scomp<-B_Privada[ c("ncomp4", "ncomp11", "ncomp18")]
scomp2<-B_Privada[ c("ncomp5", "ncomp12", "ncomp19")]
asesor<-B_Privada[ c("ncomp6", "ncomp13", "ncomp20")]
propiedad<-B_Privada[ c("ncomp7", "ncomp14", "ncomp21")]

B_Privada <- B_Privada %>%
  mutate(
    correct_response = rowSums(ncomp, na.rm = T),
    obliga =rowSums(obliga, na.rm = TRUE ),
    inicio =rowSums(inicio, na.rm = TRUE ),
    elegir =rowSums(elegir, na.rm = TRUE ),
    sirve.scmp =rowSums(sirve.scomp, na.rm = TRUE ),
    scmp.twice =rowSums(scomp2, na.rm = TRUE ),
    asesor =rowSums(asesor, na.rm = TRUE ),
    propiedad =rowSums(propiedad, na.rm = TRUE ),
    
  )

View(B_Privada[, c("sirvescomp", "scomptwice")])

B_Privada <- B_Privada %>%
  select(everything(), -contains("comp"))
rm(ncomp)




# Encuesta B Publica, computo de respuestas correctas

B_Publica <- B_Publica %>%
  mutate(
    ncomp1 = ifelse(Comp1.PP=="A partir de los 65 años", 1, ifelse(is.na(Comp1.PP), NA, 0)),
    ncomp2 = ifelse(Comp2.PP=="Tener ClaveÚnica", 1, ifelse(is.na(Comp2.PP), NA, 0)),
    ncomp3 = ifelse(Comp3.PP=="Sí", 1, ifelse(is.na(Comp3.PP), NA, 0)),
    ncomp4 = ifelse(Comp4.PP=="A través del Estado con los impuestos", 1, ifelse(is.na(Comp4.PP), NA, 0)),
    ncomp5 = ifelse(Comp5.PP=="Por encontrarse dentro del 9% de mayores ingresos", 1, ifelse(is.na(Comp5.PP), NA, 0)),
    ncomp6 = ifelse(Comp6.PP=="No", 1, ifelse(is.na(Comp6.PP), NA, 0)),
    ncomp7 = ifelse(Comp7.PP=="$1.000.000", 1, ifelse(is.na(Comp7.PP), NA, 0))
  )

ncomp<- B_Publica %>% select(contains("ncomp"))

B_Publica <- B_Publica %>%
  mutate(
    correct_response = rowSums(ncomp, na.rm = T)
  ) 

rm(ncomp)






##########################
# Merge surveys B y C
###########################

#Merge 4 surveys that collect the state 3 data
segundas_encuestas <- bind_rows(B_Privada, B_Publica,
                                C_Publica, C_Privada)
segundas_encuestas$dummy_third<-"Yes_third"
#Merge with the 1st stage data
encuestas <- encuestaA_online %>%
  left_join(segundas_encuestas, by = c("UID" = "UID")) %>%
  mutate(attrition= ifelse(is.na(StartDate.y), "Left website", "Returned to survey"),
         dup=duplicated(UID)
         ) 

#idetendify duplicates
dups<-c(encuestas[encuestas$dup==T, "UID" ])
dp<-dups[[1]]


en<- encuestas[!encuestas$UID %in% dp, ]

dups<- encuestas[encuestas$UID %in% dp, ]

# Selecting across multiple repsonses by the same person
dps<-dups %>%
  group_by(UID) %>% #### selecting the option with the most progress
  filter(Progress.y==max(Progress.y)) %>%
  ungroup() %>%
  group_by(UID) %>% 
  filter(StartDate.y==min(StartDate.y)) %>% ### and first observation of the one with the most progress
  ungroup() 

rbind

encuestas<-rbind(en, dps)
rm(en, dps)

saveRDS(encuestas, paste0(path_datos, "encuestas_clean.rds"))

#### Merge with website process data

#Merge with the 1st stage data
df <- encuestas %>%
  left_join(sitios2, by = c("UID" = "userid")) %>%
  mutate(dup2=duplicated(UID),
        ) 

saveRDS(df, paste0(path_datos, "merged_data.rds"))







