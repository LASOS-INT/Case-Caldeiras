# packs = c('ggplot2', 'cowplot', 'randomForest',
#           'caret', 'rpart.plot', 'readxl',
#           'e1071', 'AugmenterR', 'smotefamily',
#           'ROSE', 'xgboost', 'pROC', 
#           'MASS', 'lsr', 'DescTools', 
#           'dplyr', 'kernlab', 'fastAdaboost', 
#           'DataExplorer', 'dummies', 'lattice', 
#           'mlbench', 'h2o', 'here', "rattle", 
#           "MLmetrics", "ggfortify", "Rtsne", 
#           "obliqueRF", "gbm", "MLeval", 
#           "readxl", 'plotly', 'tidyverse',
#           'EnvStats', 'ggcorrplot', 'fastDummies',
#           'GGally', 'fitdistrplus', 'actuar',
#           'optmatch', 'goftest', 'triangle', 'modules')


# install_all_packages <- function () {
#   lapply(packs, install.packages, character.only = T, logical.return = TRUE)
# }


# load_library_packages <- function() {
#   lapply(packs, library, character.only = T, logical.return = TRUE)
# }


# install.packages("MASS")
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("tidyverse")
# install.packages("EnvStats")
# install.packages("ggcorrplot")
# install.packages("dplyr")
# install.packages("fastDummies")
# install.packages("GGally")
# install.packages("fitdistrplus")
# install.packages("actuar")
# install.packages("optmatch")
# install.packages("goftest")
# install.packages("modules")
# install.packages("here")


library(MASS)
library(readxl)
library(ggplot2) # graficos
library(plotly) # graficos
library(tidyverse) # um pouco de tudo /gera tibbles/dataframes
library(EnvStats)
library(ggcorrplot)
library(dplyr) # agrupar e somar os dados
library(fastDummies) # cria col. fict?cias a partir de colunas categ?ricas
library(GGally)# permite construir uma grande matriz de dispers?o
library(fitdistrplus) # distribuicoes
library(actuar) # distribuicoes
library(optmatch) # distribuicoes
library(goftest) # distribuicoes
library(triangle) # distribuicoes
library(modules)
library(here)


#library(EnvStats)
select <- dplyr::select


#### Leitura dos dados ####
my_data = here("Jupyter", "Data", "AnaliseAtualizada-BDCaldeiras_13Jun22_RevAndrea.csv") 
#setwd(here(C://Users//raquel.santos//Documents//GitHub//CaseCaldeiras-notebooks//Jupyter//Data//AnaliseAtualizada-BDCaldeiras_13Jun22_RevAndrea.csv))
#setwd("C://Users//iuri.santos//Downloads")
my_data<-read.csv2(my_data) #,encoding = "UTF-8")
#View(my_data) 


# getwd()
# setwd("C://Users//raquel.santos//Documents//GitHub//CaseCaldeiras-notebooks//Jupyter//Data")
# my_data<-read.csv2("AnaliseAtualizada-BDCaldeiras_13Jun22_RevAndrea.csv")