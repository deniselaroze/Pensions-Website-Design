### Encuestas ###

# Encuesta A, load and transformation

encuestaA_online <- read_csv(paste0(path_datos, "encuestaA_online.csv"))[-2,] %>% 
  mutate(fecha = lubridate::as_date(EndDate, format = "%Y-%m-%d"),
         sitioderivado = stringi::stri_sub(UID, -1),
         useridn = as.numeric(useridn)) %>%
  dplyr::filter(fecha  == "2022-05-20" | fecha  == "2022-05-27" | fecha == "2022-06-08" | fecha == "2022-06-07") %>%
  filter(is.na(useridn)==FALSE) %>%
  group_by(useridn) %>% 
  filter(Progress==max(Progress)) %>%
  ungroup() %>%
  select(-StartDate,
         -EndDate,
         -Status,
         -IPAddress,
         Progress,
         `Duration (in seconds)`,
         Finished,
         -RecordedDate,
         -ResponseId,
         -RecipientLastName,
         -RecipientFirstName,
         -RecipientEmail,
         -ExternalReference,
         -LocationLatitude,
         -LocationLongitude,
         -UserLanguage) %>% distinct()
encuestaA_online <- encuestaA_online[,colSums(is.na(encuestaA_online))<nrow(encuestaA_online)]

# Encuesta B Privada, load and transformation

encuestaB_Privada_online <- read_csv(paste0(path_datos, "encuestaB_Privada_online.csv")) %>% 
  mutate(fecha = lubridate::as_date(EndDate, format = "%Y-%m-%d")) %>%
  mutate(EndDate = lubridate::ymd_hms(EndDate),
         StartDate = lubridate::ymd_hms(StartDate),
         encuesta = "B_Privada",
         uemail = as.numeric(uemail),
         total_reward = as.numeric(total_reward)) %>%
  dplyr::filter(fecha  == "2022-05-20" | fecha  == "2022-05-27" | fecha == "2022-06-08" | fecha == "2022-06-07") %>%
  filter(is.na(uemail)==FALSE) %>%
  group_by(uemail) %>% 
  filter(Progress==max(Progress)) %>%
  ungroup() %>%
  group_by(uemail) %>% 
  filter(StartDate==min(StartDate)) %>%
  ungroup() %>%
  select(EndDate,
         StartDate,
         encuesta,
         contains("Comp"),
         uemail, 
         UID,
         Progress,
         Finished,
         `Duration (in seconds)`,
         Advisor,
         Recomendar,
         SelectMode.PV,
         Recomendar_NPS_GROUP,
         Confused,
         InfoUtil_1, 
         InfoAbruma2_1, 
         Curiosity_1, 
         Confidence_1 ,
         esfuerzo,
         total_reward) %>%
  rename(MB_Despues = SelectMode.PV)
encuestaB_Privada_online <- encuestaB_Privada_online[,colSums(is.na(encuestaB_Privada_online))<nrow(encuestaB_Privada_online)]

# Encuesta B Publica, load and transformation

encuestaB_Publica_online <- read_csv(paste0(path_datos, "encuestaB_Publica_online.csv")) %>% 
  mutate(fecha = lubridate::as_date(EndDate, format = "%Y-%m-%d")) %>%
  mutate(EndDate = lubridate::ymd_hms(EndDate),
         StartDate = lubridate::ymd_hms(StartDate),
         encuesta = "B_Publica",
         uemail = as.numeric(uemail),
         total_reward = as.numeric(total_reward))  %>%
  dplyr::filter(fecha  == "2022-05-20" | fecha  == "2022-05-27" | fecha == "2022-06-08" | fecha == "2022-06-07") %>%
  filter(is.na(uemail)==FALSE) %>%
  group_by(uemail) %>% 
  filter(Progress==max(Progress)) %>%
  ungroup() %>%
  group_by(uemail) %>% 
  filter(StartDate==min(StartDate)) %>%
  ungroup() %>%
  select(fecha,
         EndDate,
         StartDate,
         encuesta,
         contains("Comp"),
         uemail, 
         UID,
         Progress,
         Finished,
         `Duration (in seconds)`,
         InfoUtil_1, 
         InfoAbruma2_1,
         PostuBene,
         Recomendar,
         Recomendar_NPS_GROUP,
         SelectMode.PP,
         esfuerzo,
         total_reward)
encuestaB_Publica_online <- encuestaB_Publica_online[,colSums(is.na(encuestaB_Publica_online))<nrow(encuestaB_Publica_online)]

# Encuesta C Privada, load and transformation

encuestaC_Privada_online <- read_csv(paste0(path_datos, "encuestaC_Privada_online.csv")) %>% 
  mutate(fecha = lubridate::as_date(EndDate, format = "%Y-%m-%d")) %>%
  mutate(EndDate = lubridate::ymd_hms(EndDate),
         StartDate = lubridate::ymd_hms(StartDate),
         encuesta = "C_Privada",
         uemail = as.numeric(uemail), 
         total_reward = as.numeric(9200)) %>%
  dplyr::filter(fecha  == "2022-05-20" | fecha  == "2022-05-27" | fecha == "2022-06-08" | fecha == "2022-06-07") %>%
  filter(is.na(uemail)==FALSE) %>%
  group_by(uemail) %>% 
  filter(Progress==max(Progress)) %>%
  ungroup() %>%
  group_by(uemail) %>% 
  filter(StartDate==min(StartDate)) %>%
  ungroup() %>%
  select(fecha,
         EndDate,
         StartDate,
         encuesta,
         contains("Comp"),
         uemail, 
         UID,
         Progress, 
         Finished,
         `Duration (in seconds)`,
         InfoUtil_1, 
         InfoAbruma2_1,
         Recomendar,
         Recomendar_NPS_GROUP,
         SelectMode.PV,
         total_reward) %>%
  rename(MB_Despues = SelectMode.PV)
encuestaC_Privada_online <- encuestaC_Privada_online[,colSums(is.na(encuestaC_Privada_online))<nrow(encuestaC_Privada_online)]

# Encuesta C Publica, load and transformation

encuestaC_Publica_online <- read_csv(paste0(path_datos, "encuestaC_Publica_online.csv")) %>% 
  mutate(fecha = lubridate::as_date(EndDate, format = "%Y-%m-%d")) %>%
  mutate(EndDate = lubridate::ymd_hms(EndDate),
         StartDate = lubridate::ymd_hms(StartDate),
         encuesta = "C_Publica",
         uemail = as.numeric(uemail),
         total_reward = as.numeric(6500)) %>%
  dplyr::filter(fecha  == "2022-05-20" | fecha  == "2022-05-27" | fecha == "2022-06-08" | fecha == "2022-06-07") %>%
  filter(is.na(uemail)==FALSE) %>%
  group_by(uemail) %>% 
  filter(Progress==max(Progress)) %>%
  ungroup() %>%
  group_by(uemail) %>% 
  filter(StartDate==min(StartDate)) %>%
  ungroup() %>%
  select(fecha,
         EndDate,
         StartDate,
         encuesta,
         contains("Comp"),
         uemail, 
         UID, 
         Progress,
         Finished,
         `Duration (in seconds)`,
         InfoUtil_1, 
         InfoAbruma2_1,
         Recomendar,
         Recomendar_NPS_GROUP,
         SelectMode.PP,
         Confused,
         total_reward) %>%
  rename(MB_Despues = SelectMode.PP)
encuestaC_Publica_online <- encuestaC_Publica_online[,colSums(is.na(encuestaC_Publica_online))<nrow(encuestaC_Publica_online)]


# Encuesta B Privada, computo de respuestas correctas

encuestaB_Privada_online <- encuestaB_Privada_online %>%
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


ncomp<- encuestaB_Privada_online %>% select(contains("ncomp"))
encuestaB_Privada_online$correct_response <-rowSums(ncomp,na.rm = T)

encuestaB_Privada_online <- encuestaB_Privada_online %>%
  mutate(
    correct_response = rowSums(ncomp, na.rm = T)
  ) %>%
  select(everything(), -contains("comp"))
rm(ncomp)


# Encuesta B Publica, computo de respuestas correctas

encuestaB_Publica_online <- encuestaB_Publica_online %>%
  mutate(
    ncomp1 = ifelse(Comp1.PP=="A partir de los 65 años", 1, ifelse(is.na(Comp1.PP), NA, 0)),
    ncomp2 = ifelse(Comp2.PP=="Tener ClaveÚnica", 1, ifelse(is.na(Comp2.PP), NA, 0)),
    ncomp3 = ifelse(Comp3.PP=="Sí", 1, ifelse(is.na(Comp3.PP), NA, 0)),
    ncomp4 = ifelse(Comp4.PP=="A través del Estado con los impuestos", 1, ifelse(is.na(Comp4.PP), NA, 0)),
    ncomp5 = ifelse(Comp5.PP=="Por encontrarse dentro del 9% de mayores ingresos", 1, ifelse(is.na(Comp5.PP), NA, 0)),
    ncomp6 = ifelse(Comp6.PP=="No", 1, ifelse(is.na(Comp6.PP), NA, 0)),
    ncomp7 = ifelse(Comp7.PP=="$1.000.000", 1, ifelse(is.na(Comp7.PP), NA, 0))
  )

ncomp<- encuestaB_Publica_online %>% select(contains("ncomp"))

encuestaB_Publica_online <- encuestaB_Publica_online %>%
  mutate(
    correct_response = rowSums(ncomp, na.rm = T)
  ) %>%
  select(everything(), -contains("comp"))
rm(ncomp)


# Merge encuestas B y C
segundas_encuestas <- bind_rows(encuestaB_Privada_online, encuestaB_Publica_online,
                                encuestaC_Publica_online) %>%
  group_by(uemail) %>% 
  filter(StartDate==min(StartDate)) %>%
  ungroup()

# Merge total encuestas A, B y C
encuestas <- encuestaA_online %>%
  left_join(segundas_encuestas, by = c("useridn" = "uemail"))  %>%
  mutate(second = ifelse(is.na(encuesta)==TRUE, "sin segunda encuesta", encuesta))
encuestas <- encuestas[,colSums(is.na(encuestas))<nrow(encuestas)]


save(encuestas, file= paste0(path_datos, "encuestas_clean.Rdata"))
