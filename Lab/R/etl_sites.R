#get data
# individuals as observation units
#path_datos <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/online/data/"


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
contestar <- sitios2 %>%
  select(useridn, fecha, matches("^(n|y)_contestar"))  %>%
  pivot_longer(cols=c(y_contestar.p1, y_contestar.p2, y_contestar.p3, y_contestar.p4, y_contestar.p5, y_contestar.p6, y_contestar.p7, y_contestar.p8), values_to = "y_contestar") %>% select(-name) %>% distinct() %>%
  pivot_longer(cols=c(n_contestar.p1, n_contestar.p2, n_contestar.p3, n_contestar.p4, n_contestar.p7, n_contestar.p8), values_to = "n_contestar") %>% 
  select(-name) %>% 
  distinct()

contestar <- delete.na(contestar, 1)

contestar <- contestar %>%  #dplyr::select(useridn, fecha, y_contestar, n_contestar) %>%
  mutate(contesta = case_when(
    is.na(y_contestar)  ~ "C",
    is.na(n_contestar)  ~ "B",
    TRUE ~ "B/C"
  ),
  terminaron = "si") %>%
  group_by(useridn) 

sitios_complete <- contestar %>%
  full_join(sitios2, by="useridn") %>%
  mutate(terminaron = ifelse(is.na(terminaron), "no", terminaron))# %>% 
  # select(!matches("^(n|y)_contestar"))

saveRDS(sitios_complete, paste0(path_datos,"sitios_complete.rds"))



# Pivot sites for page behavior analysis

sitios_pivot <- sitios2 %>%
  dplyr::select(-`Created date`, -`Updated date`) %>%
  pivot_longer(cols = !c(userid, useridn, fecha, sitio, created_date, updated_date,
                         tiempo_sec_1click_nclick, tiempo_min_1click_nclick,
                         na_count, n_clicks, inicio_wix, ),
               names_to = "botones", 
               values_to = "time"
  ) %>%
  na.omit() %>%
  mutate(page = str_split(botones, "_", simplify = TRUE)) %>%
  mutate(site = str_split(page[,2], ".p", simplify = TRUE)) %>%
  mutate(pagina = case_when(
    site[,1] == "inicio" ~ "inicio",
    site[,1] == "contestar" ~ "contestar",
    site[,1] == "cm" ~ "comparar",
    site[,1] == "ps" ~ "pasos",
    site[,1] == "pgucona" & sitio
    == 1 ~ "perfil",
    site[,1] == "pguscona" & sitio == 1 ~ "perfil",
    site[,1] == "pgucona" & sitio == 3 ~ "perfil",
    site[,1] == "pgucona" & sitio == 8 ~ "producto",
    site[,1] == "pgucona" & sitio == 2 ~ "producto",
    site[,1] == "clickaqui" ~ "clickaqui",
    site[,1] == "pgusina" & sitio == 1 ~ "perfil",
    site[,1] == "pgusina" & sitio == 3 ~ "perfil",
    site[,1] == "pgusina" & sitio == 8 ~ "producto",
    site[,1] == "pgusina" & sitio == 2 ~ "producto",
    site[,1] == "instrucciones" ~ "instrucciones",
    site[,1] == "invsina" & sitio == 1 ~ "perfil",
    site[,1] == "invsina" & sitio == 3 ~ "perfil",
    site[,1] == "invsina" & sitio == 8 ~ "producto",
    site[,1] == "invsina" & sitio == 2 ~ "producto",
    site[,1] == "rsh" ~ "rsh",
    site[,1] == "heahorrado" ~ "perfil",
    site[,1] == "saludable" ~ "perfil",
    site[,1] == "gf" ~ "gf",
    site[,1] == "difmodmix" ~ "difmodmix",
    site[,1] == "ctie" ~ "ctie",
    site[,1] == "rv" ~ "rv",
    site[,1] == "pg" ~ "pg",
    site[,1] == "masinfo" ~ "masinfo",
    site[,1] == "novoyavivir" ~ "perfil",
    site[,1] == "pg" ~ "pg",
    site[,1] == "modmixtas" & sitio == 7 ~ "producto",
    site[,1] == "ci" ~ "c_invalidez",
    site[,1] == "cu" ~ "cu",
    site[,1] == "" & sitio == 7 ~ "producto"
  )) %>%
  separate(time, c("first_time", "second_time"), " ; ") %>%
  pivot_longer(cols = !c(userid, useridn, fecha, sitio, created_date, updated_date,
                         tiempo_sec_1click_nclick, tiempo_min_1click_nclick,
                         na_count, n_clicks, inicio_wix,
                         botones, page, site, pagina),
               names_to = "veces", 
               values_to = "boton_time"
  ) %>%
  na.omit() %>%
  mutate(
    boton = page[,1],
    boton_time = lubridate::parse_date_time(as.character(boton_time), "%Y-%m-%d %H:%M:%S")
  ) %>%
  group_by(useridn) %>%
  arrange(useridn,boton_time ) %>%
  mutate(first_click_diff = boton_time - lag(boton_time),
         video = case_when(
           boton == "cmvideoplay" ~ "video_play",
           boton == "cmvideopausa" ~ "video_pausa",
           boton == "psvideoplay" ~ "video_play",
           boton == "psvideopausa" ~ "video_pausa",
           boton == "videoplay" ~ "video_play",
           boton == "videopausa" ~ "video_pausa",
           TRUE ~ "non_video"
         ),
         date_play = lubridate::parse_date_time(as.character((ifelse(video == "video_play", as.character(boton_time), NA))), "%Y-%m-%d %H:%M:%S"),
         date_pause = lubridate::parse_date_time(as.character((ifelse(video == "video_pausa", as.character(boton_time), NA))), "%Y-%m-%d %H:%M:%S"),
         video_pausa = ifelse(video == "video_pausa", "si", "no"),
         tiempo_video = ifelse(lag(video)=="video_play", first_click_diff, NA)) 
#names(sitios_pivot)[17] <- "pagina"

# Pivot sites data
sitios_sm <- sitios_pivot %>%
  group_by(useridn, sitio, pagina) %>%
  dplyr::summarise(
    tiempo_pag = sum(first_click_diff, na.rm = TRUE),
    boton_time = boton_time,
    video = video,
    tiempo_video = tiempo_video,  na.rm = TRUE) %>%
  mutate(t_perfil = ifelse(sitio %in% c(1,3,4,6), "perfil", "producto"),
         t_video = ifelse(sitio %in% c(2,3,5,6), "video", "texto"),
         tratamientos = paste0(t_perfil, "-", t_video),
         Treatments = factor(case_when(
           sitio == 1 ~ "Perfil",
           sitio == 2 ~ "Video",
           sitio ==  3 ~ "VideoPerfil",
           sitio == 4 ~ "Perfil",
           sitio == 5 ~ "Video",
           sitio == 6 ~ "VideoPerfil",
           sitio == 7 ~ "Baseline",
           sitio == 8 ~ "Baseline"), levels = c("Baseline", "Perfil", "Video", "VideoPerfil")),
         tiempo_sin_video = ifelse(is.na(tiempo_video),tiempo_pag, tiempo_pag - tiempo_video),
         tiempo_con_video = as.numeric(tiempo_pag),
         vieron_video = as.factor(ifelse(is.na(tiempo_video), "no", "si")),
         tiempo_video =  ifelse(is.na(tiempo_video),0, tiempo_video)
  ) %>%
  mutate(pagina2 = factor(case_when(
    pagina == "perfil" ~ "Pension Type",
    pagina == "producto" ~ "Pension Type",
    pagina == "inicio" ~ "Start",
    pagina == "clickaqui" ~ "Click Here",
    pagina == "comparar" ~ "Compare",
    pagina == "pasos" ~ "Steps",
    pagina == "masinfo" | pagina == "difmodmix" | pagina == "gf" | pagina == "rsh" | pagina == "rv" | pagina == "pg" | pagina == "cu" | pagina == "c_invalidez" | pagina == "ctie" ~ "More Info",
    pagina == "instrucciones" ~ "Instructions",
    pagina == "contestar" ~ "Answer",
    TRUE ~ "More Info"
  ), levels = c("Start", "Click Here", "Pension Type", "Compare",  "Steps",  "Instructions",  "Answer", "More Info" )),
  pagina = factor(pagina, levels = c("inicio", "clickaqui", "comparar","pasos", "perfil", "producto", "masinfo", "instrucciones", "contestar", "difmodmix", "gf", "rsh", "rv", "pg", "cu", "c_invalidez", "ctie"))
  )

saveRDS(sitios_sm, paste0(path_datos, "sitios_sm.rds"))

