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


saveRDS(sitios, paste0(path_datos,"sitios_complete.rds"))



# Pivot sites for page behavior analysis
