#LIMPA AMBIENTE DE TRABALHO
rm(list=ls())

#### Biblioteca ####
library(MASS)
library(readxl)
library(ggplot2) # graficos
library(plotly) # graficos
library(tidyverse) # um pouco de tudo
library(EnvStats)
library(ggcorrplot)
library(dplyr) # agrupar e somar os dados
library(fastDummies) # cria col. fictícias a partir de colunas categóricas
library(GGally)# permite construir uma grande matriz de dispersão

select <- dplyr::select

#### Leitura dos dados ####
getwd()
setwd("C://Users//raque//Downloads")
#setwd("C://Users//iuri.santos//Downloads")
my_data<-read.csv2("AnaliseAtualizada-BDCaldeiras_16Mar_Raquel.xlsx - Raquel.csv")
View(my_data)

#### Tratamento de dados ####

my_data<-my_data %>% filter(ï..Fonte!="Proposta")

my_data[my_data$SomaDeHorasApontadas==0,]$SomaDeHorasApontadas<-1
#my_data[my_data$Andrea_DEPARTAMENTO.Descricao=="TRAÃ???AGEM",]$Andrea_DEPARTAMENTO.Descricao<-"TRACAGEM"
my_data$Andrea_DEPARTAMENTO.Descricao<-toupper(my_data$Andrea_DEPARTAMENTO.Descricao)

my_data<-my_data %>%mutate(OFFEscopo=as.factor(paste(OFFEscopo)),
                  OFFCod=as.factor(paste(OFFCod)),
                  Andrea_DEPARTAMENTO.Descricao=as.factor(paste(Andrea_DEPARTAMENTO.Descricao)),
                  TipodeProduto=as.factor(paste(TipodeProduto)),
                  PRODUTOS.Codigo=as.factor(paste(PRODUTOS.Codigo)),
                  Produto.Nome=as.factor(paste(Produto.Nome)),
                  ITENS.Codigo=as.factor(paste(ITENS.Codigo)),
                  ITENS.Descricao=as.factor(paste(ITENS.Descricao)),
                  SomaDeHorasApontadas=as.numeric(SomaDeHorasApontadas),
                  Poder.Linear=as.numeric(Poder.Linear),
                  Poder.Z=as.numeric(Poder.Z),
                  Cap.Normal.Linear=as.numeric(Cap.Normal.Linear),
                  Cap.Normal.Z=as.numeric(Cap.Normal.Z),
                  Evap.Normal.Linear=as.numeric(Evap.Normal.Linear),
                  Evap.Normal.Z=as.numeric(Evap.Normal.Z),
                  EvaporaÃ.Ã.o.T.H=as.numeric(EvaporaÃ.Ã.o.T.H.))

my_data<-my_data[!is.na(my_data$SomaDeHorasApontadas),]

my_data$SomaDeHorasApontadasUnitario<-my_data$SomaDeHorasApontadas/my_data$QuantidadePedida
#my_data$SomaDeHorasApontadasUnitario<-NULL #Para apagar

summary(my_data)

my_data$Produto.Nome<-as.character(paste(my_data$Produto.Nome)) ### ISSO TAVA ERRADO!!! VOCE TAVA SUBSTUINDO O MY_DATA
my_data[my_data$Produto.Nome=="CALDEIRA COMPACTA",]$Produto.Nome<-"CC"
my_data$Produto.Nome<-as.factor(paste(my_data$Produto.Nome)) ### ISSO TAVA ERRADO!!! VOCE TAVA SUBSTUINDO O MY_DATA

#### filtros####

#BoxplotItens
main_descricao<-my_data %>% filter(ITENS.Codigo!="") %>%  
  group_by(ITENS.Codigo, Andrea_DEPARTAMENTO.Descricao) %>% #juntando os iguais
  count( sort = TRUE) %>% head(n=15) %>% droplevels()
main_descricao$Concatena=paste(main_descricao$ITENS.Codigo, main_descricao$Andrea_DEPARTAMENTO.Descricao)
my_data$Concatena=paste(my_data$ITENS.Codigo, my_data$Andrea_DEPARTAMENTO.Descricao)

main_descricao<-my_data %>% filter(Concatena %in% main_descricao$Concatena) %>% droplevels() #%>%

## itens mais frequentes

mais_frequentes<-my_data %>% filter(ITENS.Codigo!="") %>%  
  group_by(ITENS.Codigo, Andrea_DEPARTAMENTO.Descricao) %>% #juntando os iguais
  count( sort = TRUE) %>% filter(n>=10)#%>% head(n=30) %>% droplevels()
mais_frequentes<-my_data %>%
  filter(paste(TipodeProduto)=="Caldeira" & ITENS.Codigo %in% mais_frequentes$ITENS.Codigo) %>%
  select(-ï..Fonte,-OFFCod,-OFFEscopo,-SomaDeHorasApontadas,-ITENS.Descricao,
         Poder.Z,-Capacidade.TSS.D.,-Cap.Normal.Linear,-Cap.Normal.Z,
         -EvaporaÃ.Ã.o.T.H.,-Evap.Normal.Linear,
         -Evap.Normal.Z,-EvaporaÃ.Ã.o.T.H,-Concatena)
mais_frequentes$Produto.Nome<-paste(mais_frequentes$Produto.Nome)
mais_frequentes<-mais_frequentes %>% mutate(Produto.Nome=ifelse(Produto.Nome=="CALDEIRAS NÃfO DEFINIDAS","CALD",
                                            ifelse(Produto.Nome=="CALDEIRA DE RECUPERAÃ???ÃfO QUÃ\u008dMICA","CALDQUIM",
                                                   ifelse(Produto.Nome=="CALDEIRA P/ QUEIMA DE COMBUSTÃ\u008dVEL SÃ"LIDO","CALDCOMB",
                                                          ifelse(Produto.Nome=="CALDEIRA COMPACTA","CALDCOMP",
                                                                 ifelse(Produto.Nome=="CC","CALDCOMP",
                                                                        ifelse(Produto.Nome=="CALDEIRA DE RECUPERAÃ???ÃfO DE CALOR, HRSG","CALDHRSG",
                                                                               ifelse(Produto.Nome=="CALDEIRA PARA QUEIMA DE Ã"LEO E GÃ\u0081S","CALDOG",
                                                                                      ifelse(Produto.Nome=="OUTROS EQUIPAMENTOS DA Ã\u0081REA DE CALDEIRAS","OUTROS",
                                                                                             ifelse(Produto.Nome=="OUTROS EQUIPAMENTOS DE PROCESSO","OUTROS",
                                                                                                    ifelse(Produto.Nome=="CILINDRO DE CLORO","CILINDRO",
                                                                                                           ifelse(Produto.Nome=="TANQUE, DUTO E CHAMINÃ???","TANQUE",
                                                                                                                  ifelse(Produto.Nome=="REATOR, VASO E TORRE","REATOR",
                                                                                                                         ifelse(Produto.Nome=="TROCADOR DE CALOR","TROC",
                                                                                                                                ifelse(Produto.Nome=="PRESTAÃ???ÃfO DE SERVIÃ???OS EM GERAL","OUTROS",Produto.Nome
                                                                                                                                       )))))))))))))))

colnames(mais_frequentes)<-c("Depart","Qtd","Prod.Tipo","Prod.Cod","Prod.Nome","Item.Cod","Poder.Lin","Poder.Z","HorasUnit")
mais_frequentes<-mais_frequentes %>% droplevels()
levels(mais_frequentes$Prod.Tipo)
mais_frequentes<-mais_frequentes %>% select(-Prod.Cod,-Prod.Tipo)

mais_frequentes<-mais_frequentes[, c("HorasUnit", "Qtd", "Poder.Z", "Poder.Lin","Depart", "Prod.Nome", "Item.Cod")]

dummies_frequentes<-dummy_cols(mais_frequentes)
dummies_frequentes<-dummies_frequentes %>% select(-Prod.Nome,-Item.Cod,-Depart)

#### Pontos ####
#soma de horas num eixo
#quantidade pedida
#poder linear

# 3 lugares solda, tracaem, montagem
# 3 produtos caldeiras, trocador de calor e vasos
clean_data <- my_data[!is.na(my_data$Poder.Linear),]
clean_data <- clean_data %>% filter((paste(TipodeProduto)=="Caldeira" | 
                                       paste(TipodeProduto)=="Trocador de Calor" |
                                       paste(TipodeProduto)=="Vasos") & 
                                      paste(Andrea_DEPARTAMENTO.Descricao)=="MONTAGEM" | 
                                      paste(Andrea_DEPARTAMENTO.Descricao)=="SOLDA" |
                                      paste(Andrea_DEPARTAMENTO.Descricao)=="TRACAGEM")

clean_data <- droplevels(clean_data)
#GPoderHorasUni

clean_media<-clean_data%>%
  group_by(TipodeProduto,Andrea_DEPARTAMENTO.Descricao,QuantidadePedida)%>%
  summarise(SomaDeHorasApontadasUnitario=mean(SomaDeHorasApontadasUnitario),Poder.Linear=mean(Poder.Linear))



#summary(my_data)

#### Criando dummies ####
# Create dummy variables:

my_dummies <-my_data%>%dplyr::select(-Concatena)

my_dummies <- dummy_cols(my_dummies,select_columns = c("Andrea_DEPARTAMENTO.Descricao",
                                                       "TipodeProduto","Produto.Nome"))
my_dummies<-my_dummies %>% dplyr::select(-OFFCod,-OFFEscopo,         
                                         -Andrea_DEPARTAMENTO.Descricao,-TipodeProduto,
                                         -Produto.Nome,-ITENS.Codigo,-ITENS.Descricao,-PRODUTOS.Codigo,-`ï..Fonte`,
                                         -Cap.Normal.Z,-Capacidade.TSS.D.,-Poder.Z,-Cap.Normal.Linear,
                                         -EvaporaÃ.Ã.o.T.H.,-Evap.Normal.Z,-Evap.Normal.Linear,-EvaporaÃ.Ã.o.T.H)
colnames(my_dummies)<-c("Tempo", "Quant","Poder","TempoUni",
                        "DEP.CONFORMACAO","DEP.MONTAGEM","DEP.SOLDA","DEP.TRACAGEM","DEP.USINAGEM",
                        "Tipo.Caldeira","Tipo.Cilindro","Tipo.Out","Tipo.Precip","Tipo.Troca","Tipo.Turbina","Tipo.Vaso",
                        "Prod.CALDEIRAHRSG","Prod.CALDQUIM",
                        "Prod.CALDCOMB","Prod.CALDOLEO",
                        "Prod.CALDNDF","Prod.Nome_CALDEIRACOMPACTA",
                        "Prod.CILINDRO","Prod.OUTROSCALD",
                        "Prod.OUTROSPROC","Prod.SERV",         
                        "Prod.REATVASOTORRE","Prod.TANQUE",
                        "Prod.TROC","Prod.TURB")#"Prod.PRECIP" #renomiando


#### criando filtros por caldeira e item codigo ####

#Item 1132 - caldeira
my_filter1132<- my_data %>% filter(ITENS.Codigo=="1132" & TipodeProduto=="Caldeira") %>%  #| ITENS.Codigo=="1152" | ITENS.Codigo=="1122") %>%
  droplevels() #usada para eliminar níveis não utilizados de um factor ou de fatores em um quadro de dados
my_filter1132$ITENS.Codigo <- droplevels(my_filter1132$ITENS.Codigo)

#Item 1152 - caldeira
my_filter1152<- my_data %>% filter(ITENS.Codigo=="1152" & TipodeProduto=="Caldeira") %>%  
  droplevels()
my_filter1152$ITENS.Codigo <- droplevels(my_filter1152$ITENS.Codigo)

#Item 1122 - caldeira
my_filter1122<- my_data %>% filter(ITENS.Codigo=="1122" & TipodeProduto=="Caldeira") %>%  
  droplevels()
my_filter1122$ITENS.Codigo <- droplevels(my_filter1122$ITENS.Codigo)


#Caldeiras - gráfico
my_caldeira<-filter(my_data,TipodeProduto=="Caldeira") 
my_caldeira<- my_caldeira %>% dplyr::select(SomaDeHorasApontadas, Produto.Nome,
                              QuantidadePedida, Poder.Linear,
                              SomaDeHorasApontadasUnitario) ## voce nao tava guardando o my_caldeira depois do select


#(GGally)
#ggcorr(my_dummies)#função de visualização para plotar uma matriz de correlação
#ggcorr(my_dummies, nbreaks = 5) #nbreakrs: especifica quantas quebras devem estar 
#contidas na escala de cores
#ERRO ggcorrplot(my_dummies, hc.order = TRUE, type = "lower",
           #lab = TRUE)

 


# Gdepartamentos
# Gprodutos
# GdensidadeProdutos
# GboxProduto
# GboxProdutoDepartamento
# GPoderHorasUni
# GPoderHoras2
# BoxplotItens
 

#### Histograma usando plotly ####

Gdepartamentos<-plot_ly(data=my_data,x=my_data$SomaDeHorasApontadasUnitario,color=my_data$Andrea_DEPARTAMENTO,type = "histogram",alpha = 0.6)%>%
  layout(barmode = "overlay")
Gprodutos<-plot_ly(data=my_data,x=my_data$SomaDeHorasApontadasUnitario,color=my_data$TipodeProduto,type = "histogram",alpha = 0.6)%>%
  layout(barmode = "overlay")


 
#mais_frequentes<- mais_frequentes %>% filter(Prod.Nome=="CALDEIRA P/ QUEIMA DE COMBUSTÃ\u008dVEL" | Prod.Nome=="CALDEIRA PARA QUEIMA DE ÃLEO E GÃ\u0081S") %>%  #& TipodeProduto=="Caldeira") %>%  #
 # droplevels()
Gprodutonome<-plot_ly(data=mais_frequentes,x=mais_frequentes$HorasUnit,color=mais_frequentes$Prod.Nome,type = "histogram",alpha = 0.6)%>%
  layout(barmode = "overlay") 

Gitemcod<-plot_ly(data=mais_frequentes,x=mais_frequentes$HorasUnit,color=mais_frequentes$Item.Cod,type = "histogram",alpha = 0.6)%>%
  layout(barmode = "overlay")

my_coletor<- my_data %>% filter(ITENS.Codigo=="1129" | ITENS.Codigo=="1131" | ITENS.Codigo=="1151") %>%  #& TipodeProduto=="Caldeira") %>%  #
  droplevels() 
my_coletor$ITENS.Codigo <- droplevels(my_raquel$ITENS.Codigo)
plot_ly(data=my_coletor,x=my_coletor$SomaDeHorasApontadasUnitario,color=my_coletor$ITENS.Codigo,type = "histogram",alpha = 0.6)%>%
  layout(barmode = "overlay")

my_tubos<- my_data %>% filter(ITENS.Codigo=="1113" | ITENS.Codigo=="1123" | ITENS.Codigo=="1132" | ITENS.Codigo=="1152" | ITENS.Codigo=="1600") %>%  
  droplevels() 
my_tubos$ITENS.Codigo <- droplevels(my_tubos$ITENS.Codigo)
plot_ly(data=my_tubos,x=my_tubos$SomaDeHorasApontadasUnitario,color=my_tubos$ITENS.Codigo,type = "histogram",alpha = 0.6)%>%
  layout(barmode = "overlay")

my_linkpipe<- my_data %>% filter(ITENS.Codigo=="1134" | ITENS.Codigo=="1154") %>%  
  droplevels() 
my_linkpipe$ITENS.Codigo <- droplevels(my_linkpipe$ITENS.Codigo)
plot_ly(data=my_linkpipe,x=my_linkpipe$SomaDeHorasApontadasUnitario,color=my_linkpipe$ITENS.Codigo,type = "histogram",alpha = 0.6)%>%
  layout(barmode = "overlay")


#### Densidades usando plotly ####  

densidadeCaldeira<-density(my_data$SomaDeHorasApontadasUnitario[paste(my_data$TipodeProduto)=='Caldeira'])
densidadeTrocador<-density(my_data$SomaDeHorasApontadasUnitario[paste(my_data$TipodeProduto)=='Trocador de Calor'])
densidadVasos<-density(my_data$SomaDeHorasApontadasUnitario[paste(my_data$TipodeProduto)=='Vasos'])
GdensidadeProdutos<-plot_ly(x = ~densidadeTrocador$x, y = ~densidadeCaldeira$y,, type = 'scatter', mode = 'lines', name = 'Caldeira', fill = 'tozeroy') %>%
  add_trace(x = ~densidadeTrocador$x, y = ~densidadeTrocador$y, name = 'Trocador', fill = 'tozeroy') %>%
  add_trace(x = ~densidadVasos$x, y = ~densidadVasos$y, name = 'Vasos', fill = 'tozeroy') %>%
  layout(title="Duração",xaxis=list(title=FALSE),
         yaxis = list(title = 'Densidade'))


#### Boxplot usando plotly ####  

GboxProduto<-plot_ly(data=my_data,x=my_data$TipodeProduto,y=my_data$SomaDeHorasApontadasUnitario,
        color = my_data$TipodeProduto,type = "box") %>%
  layout(yaxis = list(title="Duração"), title="Boxplot")#, showlegend = FALSE)

GboxProdutoDepartamento<-plot_ly(data=my_data,x=my_data$TipodeProduto,y=my_data$SomaDeHorasApontadasUnitario,
        color = my_data$Andrea_DEPARTAMENTO.Descricao,type = "box") %>%
  layout(boxmode = "group",yaxis = list(title="Duração"), title="Boxplot")#, showlegend = FALSE)

GboxProdutoNome<-plot_ly(data=my_data,x=my_data$Produto.Nome,y=my_data$SomaDeHorasApontadasUnitario,
                                 color = my_data$Andrea_DEPARTAMENTO.Descricao,type = "box") %>%
  layout(boxmode = "group",yaxis = list(title="Duração"), title="Boxplot")#, showlegend = FALSE)

#### Gráfico usando plotly ####

#GPoderHorasUni
GPoderHorasUni<-plot_ly(clean_data, x = ~SomaDeHorasApontadasUnitario, y = ~Poder.Linear,
                        color = ~TipodeProduto, symbol = ~Andrea_DEPARTAMENTO.Descricao, type = 'scatter',
                        mode = 'markers')
###### ISSO É grafico:
GPoderHoras2<-plot_ly(clean_media, x = ~SomaDeHorasApontadasUnitario, y = ~Poder.Linear,
                      color = ~TipodeProduto, symbol = ~Andrea_DEPARTAMENTO.Descricao, type = 'scatter',
                      mode = 'markers') 


#BoxplotItens
BoxplotItens<-plot_ly(data=main_descricao,x=~ITENS.Codigo,y=~SomaDeHorasApontadasUnitario,
        color = ~Andrea_DEPARTAMENTO.Descricao,type = "box")%>%
  layout(boxmode = "group",yaxis = list(title="Duração"), title="Boxplot")#, showlegend = FALSE)


BoxplotTodosItens<-plot_ly(data=my_data,x=~ITENS.Codigo,y=~SomaDeHorasApontadasUnitario,
                      color = ~Andrea_DEPARTAMENTO.Descricao, type = "box")%>%
  layout(boxmode = "group",yaxis = list(title="Duração"), title="Boxplot")#, showlegend = FALSE)


plot_ly(data=my_data,x=my_data$Andrea_DEPARTAMENTO.Descricao,y=my_data$SomaDeHorasApontadasUnitario,
       color = my_data$TipodeProduto,type = "box") %>%
 layout(boxmode = "group",yaxis = list(title="Duração"), title="Boxplot")#, showlegend = FALSE


#gráficos caldeiras e item código
#ggplot(my_filter1152,aes(x = Poder.Linear,y = SomaDeHorasApontadasUnitario, colour= Andrea_DEPARTAMENTO.Descricao)) +
 # geom_count()+geom_smooth(method=lm,se=FALSE)

#ggplot(my_filter1122,aes(x = Poder.Linear,y = SomaDeHorasApontadasUnitario, colour= Andrea_DEPARTAMENTO.Descricao)) +
#  geom_count()+geom_smooth(method=lm,se=FALSE)

#### Violinplot ####

Gviolinplot <- my_data %>% filter(Concatena %in% main_descricao$Concatena) %>% droplevels() %>%
  plot_ly(
    x = ~ITENS.Codigo,
    y = ~SomaDeHorasApontadasUnitario,
    split = ~ITENS.Codigo,
    type = 'violin',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) 

Gviolinplot <- Gviolinplot %>%
  layout(
    xaxis = list(
      title = "ITENS.Codigo"
    ),
    yaxis = list(
      title = "SomaDeHorasApontadasUnitario",
      zeroline = F
    )
  )

Gviolinplot 
#https://plotly.com/r/violin/


#### Correlograma #######
#ggcorrplot(my_data, hc.order = TRUE, type = "lower",
#           lab = TRUE)


#(GGally)
ggcorr(my_dummies)#função de visualização para plotar uma matriz de correlação
ggcorr(my_dummies, nbreaks = 5) #nbreakrs: especifica quantas quebras devem estar 
#contidas na escala de cores
#ggcorrplot(my_dummies, hc.order = TRUE, type = "lower",
#           lab = TRUE)


correlacoesPODER <- cor(my_dummies[!is.na(my_dummies$Poder),])
correlacoesNA <- cor(my_dummies[is.na(my_dummies$Poder),])
ggcorrplot(correlacoesPODER) #matriz de correlação
ggcorrplot(correlacoesNA)
#http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
#https://briatte.github.io/ggcorr/

### correlações para caldeiras mais frequentes
correlacoesFREQUENTES <- cor(dummies_frequentes)#[!is.na(my_dummies$Poder),])
ggcorr(dummies_frequentes, nbreaks = 5)
correlacoesFREQUENTESNotNA <- cor(dummies_frequentes[!is.na(dummies_frequentes$Poder.Lin),])
ggcorrplot(correlacoesFREQUENTESNotNA)
ggcorr(dummies_frequentes[!is.na(dummies_frequentes$Poder.Lin),], nbreaks = 5)

correlacoesFREQUENTESNotNA['Poder.Z',"HorasUnit"]

correlacoesFREQUENTESNotNA['Poder.Lin',"HorasUnit"]


#tudo
#Correlograma com ggpairs()
my_data %>% dplyr::select(SomaDeHorasApontadas, Andrea_DEPARTAMENTO.Descricao,
                   QuantidadePedida, TipodeProduto, Poder.Linear,
                   SomaDeHorasApontadasUnitario)  %>%
  ggpairs() #matriz de gráfico de dispersão
#Variáveis numéricas na parte esquerda da figura. 
#A correlação de Pearson à direita. 
#A distribuição variável está disponível na diagonal.

#Departamento
my_data %>% dplyr::select(SomaDeHorasApontadas, Andrea_DEPARTAMENTO.Descricao,
                   QuantidadePedida, Poder.Linear,
                   SomaDeHorasApontadasUnitario)  %>%
  ggpairs(#iris,                 # Data frame
        #columns = 1:4,        # Columns
        aes(color = Andrea_DEPARTAMENTO.Descricao,  # Color by group (cat. variable)
            alpha = 0.5))     # Transparency

#TipodeProduto
my_data %>% dplyr::select(SomaDeHorasApontadas, TipodeProduto,
                          QuantidadePedida, Poder.Linear,
                          SomaDeHorasApontadasUnitario)  %>%
  ggpairs(#iris,                 # Data frame
    #columns = 1:4,        # Columns
    aes(color = TipodeProduto,  # Color by group (cat. variable)
        alpha = 0.5))     # Transparency


# com dummies
my_data %>% dplyr::select(Andrea_DEPARTAMENTO.Descricao,
                          TipodeProduto,
                          QuantidadePedida, Poder.Linear,
                          SomaDeHorasApontadasUnitario) %>%
  dummy_cols(select_columns = c("Andrea_DEPARTAMENTO.Descricao")) %>%
  ggpairs(#iris,                 # Data frame
    #columns = 1:4,        # Columns
    aes(color = TipodeProduto,  # Color by group (cat. variable)
        alpha = 0.5))     # Transparency


#Caldeiras - gráfico
ggpairs(my_caldeira,                  #Data frame
    #columns = 1:4,        # Columns
    aes(color = Produto.Nome, # Color by group (cat. variable)
        alpha = 0.5))     # Transparency


#### Regressão ####


## grafico (esse ta certo, mas o resto iria pra cima)
ggplot(my_filter1132,aes(x = Poder.Linear,y = SomaDeHorasApontadasUnitario, colour= Andrea_DEPARTAMENTO.Descricao)) +
  geom_count()+geom_smooth(method=lm,se=FALSE)
#geom_count() = uma maneira de plotar duas variáveis que não são contínuas
#geom_smooth()= insere uma linha vermelha tracejada com ajuste linear definido pelo método (method="lm").

#ggplot(my_filter1152,aes(x = Poder.Linear,y = SomaDeHorasApontadasUnitario, colour= Andrea_DEPARTAMENTO.Descricao)) +
  #geom_count()+geom_smooth(method=lm,se=FALSE)

#ggplot(my_filter1122,aes(x = Poder.Linear,y = SomaDeHorasApontadasUnitario, colour= Andrea_DEPARTAMENTO.Descricao)) +
  #geom_count()+geom_smooth(method=lm,se=FALSE)




#### outro filtro ####

#Filtro para todos os itens
my_filterItens<-my_data %>% filter(ITENS.Codigo=="Caldeira") 
#my_filterItens<- my_filterItens %>% dplyr::select(SomaDeHorasApontadasUnitario, ITENS.Codigo, Produto.Nome,
  #                                            Andrea_DEPARTAMENTO.Descricao, Poder.Linear)
teste<-plot_ly(data=my_filterItens,x=~ITENS.Codigo,y=~SomaDeHorasApontadasUnitario,
                      color = ~Andrea_DEPARTAMENTO.Descricao,type = "box")%>%
  layout(boxmode = "group",yaxis = list(title="Duração"), title="Boxplot")#, showlegend = FALSE)


################## distribuicoes ##############

library(fitdistrplus) # distribuicoes
library(actuar) # distribuicoes
library(optmatch) # distribuicoes
library(goftest) # distribuicoes
library(triangle) # distribuicoes
library(EnvStats)


#setwd("C://Users//iuri.santos//Downloads//Distribuicoes")
setwd("C://Users//raque//Desktop//Distribuicoes")

# seleciona atividades
f=my_data

#f=dados[dados$REALIZADO==0,]
#f=f3a
tipoProduto <- unique(my_data$TipodeProduto)
length(tipoProduto)
tipoProduto
levels(tipoProduto)
for (j in 1:2){
  for (i in 0:2){
    print(j)
    if(i==0){
      filtrado = f
      grupoatividade=''
    }
    else if(i==1){
      filtrado = f[paste(f$TipodeProduto) == 'Caldeira',]#7
      grupoatividade='Caldeira'
    }
    else if(i==2){
      filtrado = f[paste(f$TipodeProduto) != 'Caldeira',]
      grupoatividade='Outros'
    }
    if (j==1){
      if(i==0){
        NomeArquivo = paste("DURACAO - ComOutlier") #Nome do arquivo com identificação do tipo de teste
      }
      else {
        NomeArquivo = paste("DURACAO GA -",grupoatividade, "- ComOutlier") #Nome do arquivo com identificação do tipo de teste
      }
    }
    else{
      if(i==0){
        NomeArquivo = paste("DURACAO - SemOutlier") #Nome do arquivo com identificação do tipo de teste
      }
      else{
        NomeArquivo = paste("DURACAO GA -",grupoatividade, "- SemOutlier") #Nome do arquivo com identificação do tipo de teste
      }
    }
    filtrado$SomaDeHorasApontadasUnitario<-filtrado$SomaDeHorasApontadasUnitario/1000
    #######   DETECCAO DE OUTLIER
    boxplot(filtrado$SomaDeHorasApontadasUnitario, main="DURACAO",xlab="DURACAOX", ylab="DURACAOY")
    boxplot(filtrado$SomaDeHorasApontadasUnitario, main="DURACAO",xlab="DURACAOX", ylab="DURACAOY")$out
    boxplot(filtrado$SomaDeHorasApontadasUnitario, main="DURACAO",xlab="DURACAOX", ylab="DURACAOY")$conf
    boxplot(filtrado$SomaDeHorasApontadasUnitario, main="DURACAO",xlab="DURACAOX", ylab="DURACAOY")$stats
    MED<-boxplot(filtrado$SomaDeHorasApontadasUnitario, main="Duracao",xlab="DURACAOX", ylab="DURACAOY")$stats[3,1]
    #######   FIM DETECCAO DE OUTLIER
    if (j==2){
      #######   ELIMINAçÃƒO DE OUTLIERS
      outliers<- boxplot(filtrado$SomaDeHorasApontadasUnitario, main="Duracao",xlab="DURACAOX", ylab="DURACAOY")$out
      LB<-boxplot(filtrado$SomaDeHorasApontadasUnitario, main="Duracao",xlab="DURACAOX", ylab="DURACAOY")$stats[1,1]
      UB<-boxplot(filtrado$SomaDeHorasApontadasUnitario, main="Duracao",xlab="DURACAOX", ylab="DURACAOY")$stats[5,1]
      MED<-boxplot(filtrado$SomaDeHorasApontadasUnitario, main="Duracao",xlab="DURACAOX", ylab="DURACAOY")$stats[3,1]
      filtrado<- filtrado[filtrado$SomaDeHorasApontadasUnitario >= LB,]
      filtrado<- filtrado[filtrado$SomaDeHorasApontadasUnitario<= UB,]
      #######   FIM DE ELIMINAçÃƒO DE OUTLIERS
    }
    x = as.numeric(filtrado$SomaDeHorasApontadasUnitario)
    summary(x)
    minT<-min(x)
    maxT<-max(x)
    # estima parÃ¢metros das distribuições (fit)
    fit_w   <- fitdist(x, "weibull")
    fit_g   <- fitdist(x, "gamma")
    fit_c   <- fitdist(x, "cauchy")
    fit_ln  <- fitdist(x, "lnorm")
    fit_ll  <- fitdist(x, "llogis")
    fit_ex  <- fitdist(x, "exp")
    fit_n  <- fitdist(x, "norm")
    #fit_b  <- fitdist(x, "binom",fix.arg=list(size=5), start=list(prob=0.2))
    fit_t <- fitdist(x,"triangle",method="mge",start = list(a=minT, b=maxT, c=MED),gof="CvM")
    
    #fit_ll2 <- fitdist(x, "llogis", fix.arg = list(shape=median(x)))
    #fit_n  <- fitdist(x, "normal")
    # fit_t  <- fitdist(x, "triangle", method = "qme", probs = c(0.1, 0.5, 0.9), start = list(a = 0, b = 49, c = 0.02) )
    # gera gráfico com informações das distribuições estimadas
    #pdf(paste0("C:/Users/",usuario,"/Desktop/TestsPlots_", NomeArquivo,".pdf"))
    pdf(paste0(getwd(),"/",NomeArquivo,".pdf"))
    par(mfrow=c(2,2))
    plot.legend <- c("weibull","gamma", "lognormal","llogis", "exponential", "normal")#, "triang")
    plot.dis = list(fit_w, fit_g, fit_ln, fit_ll, fit_ex, fit_n, fit_t)
    denscomp(plot.dis, legendtext = plot.legend)
    cdfcomp (plot.dis, legendtext = plot.legend)
    qqcomp  (plot.dis, legendtext = plot.legend)
    ppcomp  (plot.dis, legendtext = plot.legend)
    dev.off()
    # gera informações de goodness-of-fit
    #gofstat(plot.dis, fitnames = plot.legend)
    # Title
    cat("Tests Output\n\n", file = paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"))
    #teste de aderência
    cat("Teste de Aderencia\n", file = paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    #plnorm
    cat("plnorm\n", file = paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ks.test(x, "plnorm", meanlog=fit_ln$estimate[1], sdlog = fit_ln$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(cvm.test(x, "plnorm", meanlog=fit_ln$estimate[1], sdlog = fit_ln$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ad.test(x, "plnorm", meanlog=fit_ln$estimate[1], sdlog = fit_ln$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    # add 2 newlines
    cat("\n\n", file = paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    # pweibull
    cat("pweibull\n", file = paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ks.test(x, "pweibull", shape=fit_w$estimate[1], scale = fit_w$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(cvm.test(x, "pweibull", shape=fit_w$estimate[1], scale = fit_w$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ad.test(x, "pweibull", shape=fit_w$estimate[1], scale = fit_w$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    # add 2 newlines
    cat("\n\n", file = paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    # pllogis
    cat("pllogis\n", file = paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ks.test(x, "pllogis", shape=fit_ll$estimate[1], scale = fit_ll$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(cvm.test(x, "pllogis", shape=fit_ll$estimate[1], scale = fit_ll$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ad.test(x, "pllogis", shape=fit_ll$estimate[1], scale = fit_ll$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    # add 2 newlines
    # pgamma
    cat("pgamma\n", file = paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ks.test(x, "pgamma", shape=fit_g$estimate[1], rate = fit_g$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(cvm.test(x, "pgamma", shape=fit_g$estimate[1], rate = fit_g$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ad.test(x, "pgamma", shape=fit_g$estimate[1], rate = fit_g$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    # add 2 newlines
    
    #fit_c   <- fitdist(x/100, "cauchy")
    
    cat("cauchy\n", file = paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ks.test(x, "pcauchy", location=fit_c$estimate[1], scale = fit_g$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(cvm.test(x, "pcauchy", location=fit_g$estimate[1], scale = fit_g$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ad.test(x, "pcauchy", location=fit_g$estimate[1], scale = fit_g$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    # add 2 newlines
    # pexp
    cat("pexp\n", file = paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ks.test(x, "pexp", rate=fit_ex$estimate[1]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(cvm.test(x, "pexp", rate=fit_ex$estimate[1]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ad.test(x, "pexp", rate=fit_ex$estimate[1]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    # pnorm
    cat("norm\n", file = paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ks.test(x, "pnorm", mean=fit_n$estimate[1], sd = fit_n$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(cvm.test(x, "pnorm", mean=fit_n$estimate[1], sd = fit_n$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ad.test(x, "pnorm", mean=fit_n$estimate[1], sd = fit_n$estimate[2]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    ## ptri
    cat("tri\n", file = paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ks.test(x, "ptriangle", a=fit_t$estimate[1], b = fit_t$estimate[2], c = fit_t$estimate[3]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(cvm.test(x, "ptriangle", a=fit_t$estimate[1], b = fit_t$estimate[2], c = fit_t$estimate[3]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
    capture.output(ad.test(x, "ptriangle", a=fit_t$estimate[1], b = fit_t$estimate[2], c = fit_t$estimate[3]), file=paste0(getwd(),"/TestsResults_",NomeArquivo,".txt"), append = TRUE)
  }
}


filtrado = f[paste(f$TipodeProduto) == 'Caldeira',]
x<-rweibull(1000,shape=0.481309012411019, scale =0.679865448490322)
fitWeibull <- density(x)
x<-rgamma(1000,shape = 0.442876665439978, rate = 0.780781800279368)
fitGamma <- density(x)
fit2 <- density(filtrado$SomaDeHorasApontadasUnitario/1000)
filtrado = f[paste(f$TipodeProduto) == 'Caldeira',]

plot_ly(x = filtrado$SomaDeHorasApontadasUnitario/1000, type = "histogram", name = "Histogram") %>%
  add_trace(x = fitWeibull$x, y = fitWeibull$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Weibull Density") %>% 
  add_trace(x = fitGamma$x, y = fitGamma$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Gamma Density") %>% 
  add_trace(x = fit2$x, y = fit2$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Kernel Smoothing") %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right"))


#### regressões lineares e ridget#####

## preparo dos dados para os modelos GLM
mais_frequentes<-mais_frequentes[!is.na(mais_frequentes$Poder.Z),]
mais_frequentes <- mais_frequentes %>% group_by(Depart, Prod.Nome, Item.Cod)
mais_frequentes$linha <- 1:nrow(mais_frequentes)
trainSample <- mais_frequentes %>% sample_frac(size=.6)
outSample <- mais_frequentes[!(mais_frequentes$linha %in% trainSample$linha),] # pode ignorar, usado para criar as amostras seguintes
Samples<-sample(seq(1,2),size=nrow(outSample),replace=TRUE,prob=c(0.5,0.5)) #pode ignorar tbm
testSample<-outSample[Samples==1,]
validSample<-outSample[Samples==2,]

## primeiro modelo: GLM 
# treinamento do modelo
fitglm<-glm("HorasUnit ~ Poder.Z + Depart + Prod.Nome + Item.Cod",data=trainSample) # Adjusted R-squared:  0.235 
#fitglm<-lm("HorasUnit ~ Poder.Z + Item.Cod",data=trainSample) # Adjusted R-squared:  0.2098
sumfitln<-summary(fitglm)
sumfitln
#sumfitln$adj.r.squared
sumfitln$aic
#teste do modelo
predglm<-predict.glm(fitlm, newdata = testSample, se.fit = TRUE)
#obtenho estatisticas
res <- predglm$fit-testSample$HorasUnit
rmse <- sqrt(mean(res^2))
pred<- predglm$fit
colnames(pred) <- c('pred')
resultTest<- cbind(testSample,pred)

fig <- plot_ly(resultTest, x = ~Poder.Z, y = ~...9, type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
fig <- fig %>% add_trace(data = resultTest, x = ~Poder.Z, y = ~HorasUnit, name = 'Regression Fit', mode = 'lines', alpha = 1)
fig

## segundo modelo: GLM 
# treinamento do modelo
fitlm<-lm("HorasUnit ~ Poder.Z + Depart + Prod.Nome",data=trainSample) #Adjusted R-squared:  0.2098  -  instanciar uma regressão lienar - lm()
sumfitln<-summary(fitlm)
sumfitln$adj.r.squared
#teste do modelo
predlm<-predict(fitlm, newdata = testSample, se.fit = TRUE)
#obtenho estatisticas
res <- predlm$fit-testSample$HorasUnit
rmse <- sqrt(mean(res^2))


# terceiro modelo:
#trainSample$HorasUnitLog<-log(trainSample$HorasUnit)
#fitglm<-glm("HorasUnitLog ~ Poder.Z + Depart + Prod.Nome + Item.Cod",data=trainSample,family="Gamma") #nao ta funcionando

fitglm<-glm("HorasUnit ~ Poder.Z + Depart + Item.Cod",data=trainSample)
sumfitln<-summary(fitglm)
sumfitln$adj.r.squared
sumfitln$aic
#teste do modelo
predlm<-predict(fitlm, newdata = testSample, se.fit = TRUE)
#obtenho estatisticas
res <- predlm$fit-testSample$HorasUnit
rmse <- sqrt(mean(res^2))

### Ridge Regression
traindummies <- dummy_cols(trainSample, remove_selected_columns = TRUE, remove_first_dummy = TRUE)
testdummies <- dummy_cols(testSample, remove_selected_columns = TRUE, remove_first_dummy = TRUE)
library(glmnet)

x = as.matrix(traindummies)
y_train = trainSample$HorasUnit

x_test = as.matrix(testdummies)
y_test = testSample$HorasUnit

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)

summary(ridge_reg)
optimal_lambda <- ridge_reg$lambda.min
optimal_lambda

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, trainSample)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, testSample)






## comparo os modelos


## o melhor modelo vai ser validado na validSample.
## com essa amostra, obtenho minhas estatisticas finais

#r2
#rmse
#graficos