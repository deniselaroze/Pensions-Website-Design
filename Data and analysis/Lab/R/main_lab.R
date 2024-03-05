##################
### Denise Laroze
### March 2024
###################



rm(list=ls())

path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/Data and analysis/Lab/"
path_datos<-"C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/Data and analysis/Lab/Lab Data/Surveys and websites/"

# create folders if does´t exists

#ifelse(!dir.exists(file.path(path_github, "R")), 
#       dir.create(file.path(path_github, "R")), FALSE)

## Load packages
source(paste0(path_github,"R/paquetes.R"))
## Load own functions
source(paste0(path_github,"R/funciones.R"))
## Merge datasets
source(paste0(path_github,"R/Merge_datasets_lab.R"))
# import, transform and save site data
source(paste0(path_github,"R/Data_management_lab.R"))

# Analyse data online experiment
source(paste0(path_github,"Online Data/Data_analysis_lab.R"))
### Run "Data_analysis_online.R"
