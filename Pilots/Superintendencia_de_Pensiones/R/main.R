## Set directory
# change your path
path_datos <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/"
# If you don´t use Rprojects functionality setwd
#setwd("C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/")
path_github <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/Pilots/Superintendencia_de_Pensiones"

# create folders if doesn´t exists

ifelse(!dir.exists(file.path(path_github, "R")), 
       dir.create(file.path(path_github, "R")), FALSE)
ifelse(!dir.exists(file.path(path_github, "data")), 
       dir.create(file.path(path_github, "data")), FALSE)
ifelse(!dir.exists(file.path(path_github, "figures")), 
       dir.create(file.path(path_github, "figures")), FALSE)

## Load packages
source(paste0(path_github,"Pilots/Superintendencia_de_Pensiones/R/paquetes.R"))
## Load own functions
source(here::here("Pilots/Superintendencia_de_Pensiones/R/funciones.R"))
## ETL
# import, transform and save data surveys
source(here::here("Pilots/Superintendencia_de_Pensiones/R/etl_surveys.R"))
# import, transform and save site data
source(here::here("Pilots/Superintendencia_de_Pensiones/R/etl_sites.R"))
# complete join (surveys + sites)
source(here::here("Pilots/Superintendencia_de_Pensiones/R/etl_complete.R"))
# if you don´t use  Rprojects functionality setwd inside the Rmd
## Primary Analysis
rmarkdown::render(
  here::here("Pilots/Superintendencia_de_Pensiones/R/pilot_analysis.Rmd")
)
