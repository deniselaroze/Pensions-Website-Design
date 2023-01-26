##################
### Denise Laroze
### jan 2023
###################


rm(list=ls())



## Set directory
# change your path
#path_datos <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/"
path_datos <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Laboratorio/Encuestas y sitios/"
# If you don´t use Rprojects functionality setwd
#setwd("C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/")
#path_github <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/Pilots/Superintendencia_de_Pensiones"

path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/"



# create folders if doesn´t exists

ifelse(!dir.exists(file.path(path_github, "R")), 
       dir.create(file.path(path_github, "R")), FALSE)
ifelse(!dir.exists(file.path(path_github, "data")), 
       dir.create(file.path(path_github, "data")), FALSE)
ifelse(!dir.exists(file.path(path_github, "figures")), 
       dir.create(file.path(path_github, "figures")), FALSE)

## Load packages
source(paste0(path_github,"Lab/R/paquetes.R"))
## Load own functions
source(paste0(path_github,"Lab/R/funciones.R"))
## ETL
# import, transform and save data surveys
source(paste0(path_github,"Lab/R/etl_surveys.R"))
# import, transform and save site data
source(paste0(path_github,"Lab/R/etl_sites.R"))
# complete join (surveys + sites)
source(paste0(path_github,"Lab/R/etl_complete.R"))
# if you don´t use  Rprojects functionality setwd inside the Rmd
## Primary Analysis
#rmarkdown::render(
#  paste0(path_github, "Lab/R/lab_analysis.Rmd")
#)
