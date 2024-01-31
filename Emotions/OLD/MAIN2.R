#!/usr/bin/env Rscript
rm(list=ls());
#path <- '/Users/mauro/Desktop/Cess/Pensiones/2023/emov-main/';

path <- 'C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/Emotions';

setwd(path);
reset <- function() .rs.restartR()
clear <- function() cat("\014\n")
source("CONFIG2.R");
#source("Data_Management_Videos.R");

#Calcular regresiones
# no las dejé acá hay que correrlas manual en LOAD



#m_df <- readRDS('match_df-current.RDS')
#m_df <- select(m_df,UID,UIDx)
##colnames(m_df) <- c("UID","UIDvideo")
#View(m_df)