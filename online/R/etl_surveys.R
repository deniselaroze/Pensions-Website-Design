### Encuestas ###

# Encuesta A, load and transformation

#path_datos <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/online/data/"

encuesta_A <- "Encuesta_A.csv"

encuestaA_online <- read_csv(paste0(path_datos, encuesta_A))[-1,] %>% 
  mutate(#fecha = lubridate::as_date(EndDate, format = "%Y-%m-%d"),
         sitioderivado = case_when(
         nchar(useridn) == 4  ~ stringi::stri_sub(UID, 8, 8),
         nchar(useridn) == 3  ~ stringi::stri_sub(UID, 7, 7)),
         useridn = as.numeric(useridn)) %>%
  filter(is.na(useridn)==FALSE) %>%
  group_by(useridn) %>% 
  filter(Progress==max(Progress)) %>%
  ungroup() %>%
  select(-Status,
         -IPAddress,
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
  group_by(uemail) %>% 
  filter(Progress==max(Progress)) %>%
  ungroup() %>%
  group_by(uemail) %>% 
  filter(StartDate==min(StartDate)) %>%
  ungroup() %>%
  mutate(perfilfiltrar = ifelse(is.na(perfil), "no_filtrar", 
                                ifelse(perfil==0, "no_filtrar", "filtrar"))) %>%
  filter(perfilfiltrar != "filtrar") %>%
  select(-perfilfiltrar,
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
  group_by(uemail) %>% 
  filter(Progress==max(Progress)) %>%
  ungroup() %>%
  group_by(uemail) %>% 
  filter(StartDate==min(StartDate)) %>%
  ungroup() %>%
  mutate(perfilfiltrar = ifelse(is.na(perfil), "no_filtrar", 
                                ifelse(perfil==0, "no_filtrar", "filtrar"))) %>%
  filter(perfilfiltrar != "filtrar") %>%
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
  group_by(uemail) %>% 
  filter(Progress==max(Progress)) %>%
  ungroup() %>%
  group_by(uemail) %>% 
  filter(StartDate==min(StartDate)) %>%
  ungroup() %>%
  mutate(perfilfiltrar = ifelse(is.na(perfil), "no_filtrar", 
                                ifelse(perfil==0, "no_filtrar", "filtrar"))) %>%
  filter(perfilfiltrar != "filtrar") %>%
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
B_Privada$correct_response <-rowSums(ncomp,na.rm = T)

B_Privada <- B_Privada %>%
  mutate(
    correct_response = rowSums(ncomp, na.rm = T)
  ) %>%
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
  ) %>%
  select(everything(), -contains("comp"))
rm(ncomp)


# Merge encuestas B y C
segundas_encuestas <- bind_rows(B_Privada, B_Publica,
                                C_Publica, C_Privada) %>%
  group_by(uemail) %>% 
  filter(StartDate==min(StartDate)) %>%
  ungroup()

# Merge total encuestas A, B y C
encuestas <- encuestaA_online %>%
  left_join(segundas_encuestas, by = c("useridn" = "uemail"))  %>%
  mutate(second = ifelse(is.na(encuesta)==TRUE, "sin segunda encuesta", encuesta))
encuestas <- encuestas[,colSums(is.na(encuestas))<nrow(encuestas)]


saveRDS(encuestas, paste0(path_datos, "encuestas_clean.rds"))
