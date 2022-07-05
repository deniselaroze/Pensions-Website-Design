pilot_data <- sitios_complete %>%
  right_join(encuestas, by = "useridn") %>%
  filter(useridn !=0) %>%
  mutate(Age = 2022 - as.numeric(Birth),
         Treatments = factor(case_when(
           website == "1" ~ "Perfil",
           website == "2" ~ "Video",
           website ==  "3" ~ "VideoPerfil",
           website == "4" ~ "Perfil",
           website == "5" ~ "Video",
           website == "6" ~ "VideoPerfil",
           website == "7" ~ "Baseline",
           website == "8" ~ "Baseline"), levels = c("Baseline", "Perfil", "Video", "VideoPerfil")),
         OptOut = case_when(
           terminaron == "si" & contesta != "C" ~ "In",
           TRUE ~ "Out"
         ),
         Pension_Type = factor(case_when(
           website == "1" ~ "Public",
           website == "2" ~ "Public",
           website == "3" ~ "Public",
           website == "8" ~ "Public",
           website == "4" ~ "Private",
           website == "5" ~ "Private",
           website == "6" ~ "Private",
           website == "7" ~ "Private"), levels = c("Public", "Private")),
         online = ifelse(fecha.x.x == "2022-06-08", 1, 0)) %>% 
  arrange(useridn)

breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")

pilot_data$Time_of_day <- cut(x=hour(pilot_data$StartDate), breaks = breaks, labels = labels, include.lowest=TRUE)

saveRDS(pilot_data, here::here("Pilots/Superintendencia_de_Pensiones/data/pilot_data.rds"))