##################
### Denise Laroze
### jan 2023
###################


rm(list=ls())



## Set directory
# change your path
#path_datos <- "C:/Users/Usach/Dropbox/Sitios web/Datos Estudio Online/"
#path_datos <- "C:/Users/Denise/Dropbox/Sitios web/Datos Laboratorio/Encuestas y sitios/"
path_datos <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Laboratorio/Encuestas y sitios/"


# If you don´t use Rprojects functionality setwd
path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/"
#path_github <- "C:/Users/Denise/Documents/GitHub/Pensions-Website-Design/"



# create folders if doesn´t exists

#ifelse(!dir.exists(file.path(path_github, "R")), 
#       dir.create(file.path(path_github, "R")), FALSE)
#ifelse(!dir.exists(file.path(path_github, "data")), 
#       dir.create(file.path(path_github, "data")), FALSE)
#ifelse(!dir.exists(file.path(path_github, "figures")), 
#       dir.create(file.path(path_github, "figures")), FALSE)

## Load packages
source(paste0(path_github,"Lab/R/paquetes.R"))
## Load own functions
source(paste0(path_github,"Lab/R/funciones.R"))
# import, transform and save data surveys
source(paste0(path_github,"Lab/R/Merge_datasets.R"))
# import, transform and save site data
source(paste0(path_github,"Lab/R/Data_management.R"))
# complete join (surveys + sites)

### Run "Data_analysis_online.R"