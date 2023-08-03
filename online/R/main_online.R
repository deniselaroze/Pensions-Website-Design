##################################
### Main data management code
##################################


rm(list=ls())
## Set directory
# change your path
#path_datos <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/"
path_datos <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Estudio Online/"
# If you don´t use Rprojects functionality setwd
#setwd("C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/")
#path_github <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/Pilots/Superintendencia_de_Pensiones"

path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/"

# create folders if does´t exists

#ifelse(!dir.exists(file.path(path_github, "R")), 
#       dir.create(file.path(path_github, "R")), FALSE)

## Load packages
source(paste0(path_github,"online/R/paquetes.R"))
## Load own functions
source(paste0(path_github,"online/R/funciones.R"))
## ETL
# import, transform and save data surveys
source(paste0(path_github,"online/R/Merge_datasets.R"))
# import, transform and save site data
source(paste0(path_github,"online/R/Data_management.R"))
# complete join (surveys + sites)

### Run "Data_analysis_online.R"