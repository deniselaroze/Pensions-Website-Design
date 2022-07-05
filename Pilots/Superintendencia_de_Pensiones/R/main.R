## Set directory
# change your path
path <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/"
# If you don´t use Rprojects functionality setwd
#setwd("C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/")

# create folders if doesn´t exists

ifelse(!dir.exists(file.path(path, "R")), 
       dir.create(file.path(path, "R")), FALSE)

ifelse(!dir.exists(file.path(path, "data")), 
       dir.create(file.path(path, "data")), FALSE)

## Load packages
source(paste0(path,"Pilots/Superintendencia_de_Pensiones/R/paquetes.R"))

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
