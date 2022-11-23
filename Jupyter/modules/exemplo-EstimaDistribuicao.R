### codigo criado por Iuri Santos (iurix_ms@hotmail.com / iuri.santos@tecgraf.puc-rio.br)
# para rodar as funcoes de EstimaDistribuiçao.R
#LIMPA AMBIENTE DE TRABALHO
#rm(list=ls())

# #CHAMADA DE PACOTES externa

# install.packages("tidyverse")
# install.packages("here")
# install.packages("modules")
# install.packages("magrittr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("tibble")
# install.packages("broom")
# install.packages("fitdistrplus")
# install.packages("graphics")
# install.packages("stats")
# install.packages("dgof")
# install.packages("actuar")
# install.packages("triangle")
# install.packages("goftest")
# install.packages("grDevices")

library(tidyverse) # gera tibbles/dataframes
library(here)
library(modules)

import::from(here)
import::from(tidyverse)
import::from(modules)

select <- dplyr::select
###  para rodar o modulo, recomenda-se ter instalado os seguintes pacotes:
# magrittr,dplyr,tidyr,tibble,broom,fitdistrplus,graphics,stats,dgof,actuar,triangle,goftest,grDevices


######### LEITURA DE DADOS ##################
getwd() 
my_data = here("Jupyter", "Data", "AnaliseAtualizada-BDCaldeiras_13Jun22_RevAndrea.csv")
#setwd(here("Data", "AnaliseAtualizada-BDCaldeiras_13Jun22_RevAndrea.csv")) # "C://Users//iuri.santos//Downloads") # substituir pelo seu wd
#setwd(here(C://Users//raquel.santos//Desktop//Jupyter//Data//AnaliseAtualizada-BDCaldeiras_13Jun22_RevAndrea.csv))



# getwd()
# setwd("C://Users//raque//Documents//CaseCaldeiras-notebooks//Jupyter//Data")

### chamada de modulo
# para chamar funcao, recomendo usar como modelo
# https://cran.r-project.org/web/packages/modules/vignettes/modulesInR.html
# m <- modules::use("Jupyter/modules/estimaDistribuicao.R")
m <- ("estimaDistribuicao.R")



# ############ codigo de exemplo #############
# # gerando amostras do exemplo
# alpha = 10
# theta = 150 / 60
# # Creates dataframe
# df1 = tibble(
#   obs = 1:1000,
#   y = rgamma(n = 1000, shape = alpha, scale = theta)*100,
#   x = rweibull(n = 1000, shape = alpha, scale = theta*10)*100,
#   SomaDeHorasApontadasUnitario = append(rweibull(n = 500, shape = alpha, scale = theta*10),
#                                         rgamma(n = 500, shape = alpha, scale = theta))*100
# )
# # Creates another dataframe
# df2 = tibble(
#   obs = 1:800,
#   y = rgamma(n = 800, shape = alpha, scale = theta)*100,
#   x = rweibull(n = 800, shape = alpha, scale = theta*10)*100,
#   SomaDeHorasApontadasUnitario = append(append(rweibull(n = 300, shape = alpha, scale = theta*10),
#                                         rgamma(n = 300, shape = alpha, scale = theta)),
#                                         rlnorm(n=200, meanlog = 3.2, sdlog = .3))*100
# )
# # junta dfs numa lista
# dadosTestes<-list(df1,df2)
# # testa a funcao
# #resultadoTeste=m$estima_distribuicao(dadosTestes,TRUE, coluna='x')
# resultadoTeste=estima_distribuicao(dadosTestes,TRUE, coluna='x')
# # vendo resultado do exemplo
# resultadoTeste
# #segundo teste
# #resultadoTeste=m$estima_distribuicao(dadosTestes,TRUE)
# resultadoTeste=estima_distribuicao(dadosTestes,TRUE)
# # # vendo resultado do exemplo
# resultadoTeste
# # #terceiro teste
# #resultadoTeste=m$estima_distribuicao(dadosTestes,TRUE,quant_amostragens=2)
# resultadoTeste=estima_distribuicao(dadosTestes,TRUE,quant_amostragens=2)
# # #vendo resultado do exemplo
# resultadoTeste

# # #quarto teste
# #resultadoTeste=m$estima_distribuicao(df1,FALSE,quant_amostragens=1)
# resultadoTeste=estima_distribuicao(df1,FALSE,quant_amostragens=1)
# # # vendo resultado do exemplo
# resultadoTeste
