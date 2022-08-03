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

saveRDS(pilot_data, paste0(path_datos, "pilot_data.rds"))