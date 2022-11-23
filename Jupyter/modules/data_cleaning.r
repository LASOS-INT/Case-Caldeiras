library(here)
source(here("Jupyter","modules","bib.r")) #source(here("modules","bib.r")) #INT


#### Renomeando o nome das caldeiras

levels(my_data$Produto.Nome)[match("CALDEIRAS NAO DEFINIDAS", levels(my_data$Produto.Nome))] <- "CND"
levels(my_data$Produto.Nome)[match("CALDEIRA DE RECUPERACAO QUIMICA", levels(my_data$Produto.Nome))] <- "CRQ"

levels(my_data$Produto.Nome)[match("CALDEIRA P/ QUEIMA DE COMBUSTIVEL SOLIDO", levels(my_data$Produto.Nome))] <- "CQCS"
levels(my_data$Produto.Nome)[match("CALDEIRA COMPACTA", levels(my_data$Produto.Nome))] <- "CC"

levels(my_data$Produto.Nome)[match("CALDEIRA DE RECUPERACAO DE CALOR, HRSG", levels(my_data$Produto.Nome))] <- "CRCHRSG"
levels(my_data$Produto.Nome)[match("CALDEIRA PARA QUEIMA DE OLEO E GAS", levels(my_data$Produto.Nome))] <- "CQOG"

levels(my_data$Produto.Nome)[match("OUTROS EQUIPAMENTOS DA AREA DE CALDEIRAS", levels(my_data$Produto.Nome))] <- "OUTROS"  
levels(my_data$Produto.Nome)[match("OUTROS EQUIPAMENTOS DE PROCESSO", levels(my_data$Produto.Nome))] <- "OUTROS"                                                                

levels(my_data$Produto.Nome)[match("CILINDRO DE CLORO", levels(my_data$Produto.Nome))] <- "CILINDRO"                                                                
levels(my_data$Produto.Nome)[match("TANQUE, DUTO E CHAMINE", levels(my_data$Produto.Nome))] <- "TANQUE" 

levels(my_data$Produto.Nome)[match("REATOR, VASO E TORRE", levels(my_data$Produto.Nome))] <- "REATOR" 
levels(my_data$Produto.Nome)[match("TROCADOR DE CALOR", levels(my_data$Produto.Nome))] <- "TC"                                                                                                           
levels(my_data$Produto.Nome)[match("TPRESTACAO DE SERVICOS EM GERAL", levels(my_data$Produto.Nome))] <- "OUTROS"


##### tratamento de dados 
numeric_columns = c(
    'SomaDeHorasApontadas',
    'Poder.Linear',
    'Poder.Z',
    'Cap.Normal.Linear',
    'Cap.Normal.Z',
    'Evap.Normal.Linear',
    'Evap.Normal.Z',
    'Evaporacao.T.H.',
    'QuantidadePedida'
)
columns = names(my_data) 
categorical_columns <- columns[!columns %in% numeric_columns]

my_data$Andrea_DEPARTAMENTO.Descricao<-toupper(my_data$Andrea_DEPARTAMENTO.Descricao)
my_data[, categorical_columns] <- lapply(my_data[, categorical_columns], as.factor) # pasando para facto todos os dados categóricos
my_data[, numeric_columns] <- lapply(my_data[, numeric_columns], as.numeric) # passando para o tipo númerico

my_data$Poder.Z[is.na(my_data$Poder)]<-0
my_data<-my_data[!is.na(my_data$SomaDeHorasApontadas),]
my_data<-my_data %>% mutate(SomaDeHorasApontadasUnitario= SomaDeHorasApontadas/QuantidadePedida) # criando campo de SomdaDeHorasApontadasUnitario
#head(my_data[, numeric_columns])


#### Histograma usando plotly 

#gráfico no Jupyter
#Gdepartamentos<-plot_ly(data=my_data,x=my_data$SomaDeHorasApontadasUnitario,color=my_data$Andrea_DEPARTAMENTO,type = "histogram",alpha = 0.6)%>%
  #layout(barmode = "overlay")
#Gdepartamentos

#gráfico no Jupyter
#Gprodutos<-plot_ly(data=my_data,x=my_data$SomaDeHorasApontadasUnitario,color=my_data$TipodeProduto,type = "histogram",alpha = 0.6)%>%
  #layout(barmode = "overlay")
#Gprodutos


####  Clean data -  Gráfico de pontos
#soma de horas num eixo
#quantidade pedida
#poder linear

# # 3 lugares solda, tracaem, montagem
# # 3 produtos caldeiras, trocador de calor e vasos
clean_data <- my_data[!is.na(my_data$Poder.Linear),]
clean_data <- clean_data %>% filter((paste(TipodeProduto)=="Caldeira" | 
                                       paste(TipodeProduto)=="Trocador de Calor" |
                                       paste(TipodeProduto)=="Vasos") & 
                                      paste(Andrea_DEPARTAMENTO.Descricao)=="MONTAGEM" | 
                                      paste(Andrea_DEPARTAMENTO.Descricao)=="SOLDA" |
                                      paste(Andrea_DEPARTAMENTO.Descricao)=="TRACAGEM")

clean_data <- droplevels(clean_data)

#gráfico no Jupyter
# GPoderHorasUni<-plot_ly(clean_data, x = ~SomaDeHorasApontadasUnitario, y = ~Poder.Linear,
#                          symbol = ~Andrea_DEPARTAMENTO.Descricao, type = 'scatter',
#                         mode = 'markers')
#GPoderHorasUni

clean_media<-clean_data%>%
  group_by(TipodeProduto,Andrea_DEPARTAMENTO.Descricao,QuantidadePedida)%>%
  summarise(SomaDeHorasApontadasUnitario=mean(SomaDeHorasApontadasUnitario),Poder.Linear=mean(Poder.Linear))

#gráfico no jupyter
# GPoderHoras2<-plot_ly(clean_media, x = ~SomaDeHorasApontadasUnitario, y = ~Poder.Linear,
#                       symbol = ~Andrea_DEPARTAMENTO.Descricao, type = 'scatter',
#                       mode = 'markers') 

#GPoderHoras2

#### Densidade
#no Jupyter
# densidadeCaldeira<-density(my_data$SomaDeHorasApontadasUnitario[paste(my_data$TipodeProduto)=='Caldeira'])
# densidadeTrocador<-density(my_data$SomaDeHorasApontadasUnitario[paste(my_data$TipodeProduto)=='Trocador de Calor'])
# densidadVasos<-density(my_data$SomaDeHorasApontadasUnitario[paste(my_data$TipodeProduto)=='Vasos'])
# GdensidadeProdutos<-plot_ly(x = ~densidadeTrocador$x, y = ~densidadeCaldeira$y,, type = 'scatter', mode = 'lines', name = 'Caldeira', fill = 'tozeroy') %>%
#   add_trace(x = ~densidadeTrocador$x, y = ~densidadeTrocador$y, name = 'Trocador', fill = 'tozeroy') %>%
#   add_trace(x = ~densidadVasos$x, y = ~densidadVasos$y, name = 'Vasos', fill = 'tozeroy') %>%
#   layout(title="Duracao",xaxis=list(title=FALSE),
#          yaxis = list(title = 'Densidade'))

#GdensidadeProdutos


#### df my_data - Boxplot usando plotly  

#no Jupyter
# GboxProduto<-plot_ly(data=my_data,x=my_data$TipodeProduto,y=my_data$SomaDeHorasApontadasUnitario,
#                      color = my_data$TipodeProduto,type = "box") %>%
#   layout(yaxis = list(title="Dura??o"), title="Boxplot")#, showlegend = FALSE)

#no Jupyter
# GboxProdutoDepartamento<-plot_ly(data=my_data,x=my_data$TipodeProduto,y=my_data$SomaDeHorasApontadasUnitario,
#                                  color = my_data$Andrea_DEPARTAMENTO.Descricao,type = "box") %>%
#   layout(boxmode = "group",yaxis = list(title="Dura??o"), title="Boxplot")#, showlegend = FALSE)

#no Jupyter
# GboxProdutoNome<-plot_ly(data=my_data,x=my_data$Produto.Nome,y=my_data$SomaDeHorasApontadasUnitario,
#                          color = my_data$Andrea_DEPARTAMENTO.Descricao,type = "box") %>%
#   layout(boxmode = "group",yaxis = list(title="Dura??o"), title="Boxplot")#, showlegend = FALSE)


BoxplotTodosItens<-plot_ly(data=my_data,x=~ITENS.Codigo,y=~SomaDeHorasApontadasUnitario,
                           color = ~Andrea_DEPARTAMENTO.Descricao, type = "box")%>%
  layout(boxmode = "group",yaxis = list(title="Dura??o"), title="Boxplot")#, showlegend = FALSE)



#### criando df main_descricao

main_descricao<-my_data %>% filter(ITENS.Codigo!="") %>%  
  group_by(ITENS.Codigo, Andrea_DEPARTAMENTO.Descricao) %>% #juntando os iguais
  count( sort = TRUE) %>% head(n=15) %>% droplevels()
main_descricao$Concatena=paste(main_descricao$ITENS.Codigo, main_descricao$Andrea_DEPARTAMENTO.Descricao)
my_data$Concatena=paste(my_data$ITENS.Codigo, my_data$Andrea_DEPARTAMENTO.Descricao)
#no Jupyter
#main_descricao<-my_data %>% filter(Concatena %in% main_descricao$Concatena) %>% droplevels() #%>%
# BoxplotItens<-plot_ly(data=main_descricao,x=~ITENS.Codigo,y=~SomaDeHorasApontadasUnitario,
#         color = ~Andrea_DEPARTAMENTO.Descricao,type = "box")%>%
#   layout(boxmode = "group",yaxis = list(title="Duracao"), title="Boxplot")
#BoxplotItens

#no Jupyter
# BoxplotDepartamento<-plot_ly(data=my_data,x=my_data$Andrea_DEPARTAMENTO.Descricao,y=my_data$SomaDeHorasApontadasUnitario,
#         color = my_data$TipodeProduto,type = "box") %>%
#   layout(boxmode = "group",yaxis = list(title="Dura??o"), title="Boxplot")#, showlegend = FALSE



#### scatterplot - gáfrico de dispressão
#tudo no Jupyter
#Correlograma com ggpairs()

#Departamento
#observar a correlação dos departamentos com os campos selecionados no df
# my_data %>% dplyr::select(SomaDeHorasApontadas, Andrea_DEPARTAMENTO.Descricao,
#                           QuantidadePedida, Poder.Linear,
#                           SomaDeHorasApontadasUnitario)  %>%
#   ggpairs(aes(color = Andrea_DEPARTAMENTO.Descricao, alpha = 0.5)) #matriz de gr?fico de dispers?o
# #Vari?veis num?ricas na parte esquerda da figura. 
# #A correla??o de Pearson ? direita. 
# #A distribui??o vari?vel est? dispon?vel na diagonal.

# #TipodeProduto
# #observar a correlação do tipo de produto com os campos selecionados no df
# my_data %>% dplyr::select(SomaDeHorasApontadas, TipodeProduto,
#                           QuantidadePedida, Poder.Linear,
#                           SomaDeHorasApontadasUnitario)  %>%
#   ggpairs(aes(color = TipodeProduto, alpha = 0.5)) 


# #com dummies
# #para os dados categóricos foram criadas as dummies
# my_data %>% dplyr::select(Andrea_DEPARTAMENTO.Descricao,
#                           TipodeProduto,
#                           QuantidadePedida, Poder.Linear,
#                           SomaDeHorasApontadasUnitario) %>%
#   dummy_cols(select_columns = c("Andrea_DEPARTAMENTO.Descricao")) %>%
#   ggpairs(aes(color = TipodeProduto, alpha = 0.5)) 


# #Caldeiras 
# my_caldeira<-filter(my_data,TipodeProduto=="Caldeira") 
# my_caldeira<- my_caldeira %>% dplyr::select(SomaDeHorasApontadas, Produto.Nome,
#                                             QuantidadePedida, Poder.Linear,
#                                             SomaDeHorasApontadasUnitario) 
# ggpairs(cardinality_threshold=20, my_caldeira,                  #Data frame
#         #columns = 1:4,        # Columns
#         aes(color = Produto.Nome, # Color by group (cat. variable)
#             alpha = 0.5))     # Transparency


#### scaterplot - gráfico de disperssão
# criando filtros por caldeira e item codigo ####

#tudo no Jupyter
#Item 1132 - caldeira
# my_filter1132<- my_data %>% filter(ITENS.Codigo=="1132" & TipodeProduto=="Caldeira") %>%  #| ITENS.Codigo=="1152" | ITENS.Codigo=="1122") %>%
#   droplevels() #usada para eliminar n?veis n?o utilizados de um factor ou de fatores em um quadro de dados
# my_filter1132$ITENS.Codigo <- droplevels(my_filter1132$ITENS.Codigo)
# ggplot(my_filter1132,aes(x = Poder.Linear,y = SomaDeHorasApontadasUnitario, colour= Andrea_DEPARTAMENTO.Descricao)) +
#   geom_count()+geom_smooth(method=lm,se=FALSE)

# #Item 1152 - caldeira
# my_filter1152<- my_data %>% filter(ITENS.Codigo=="1152" & TipodeProduto=="Caldeira") %>%  
#   droplevels()
# my_filter1152$ITENS.Codigo <- droplevels(my_filter1152$ITENS.Codigo)
# ggplot(my_filter1152,aes(x = Poder.Linear,y = SomaDeHorasApontadasUnitario, colour= Andrea_DEPARTAMENTO.Descricao)) + geom_count()+geom_smooth(method=lm,se=FALSE)

# #Item 1122 - caldeira
# my_filter1122<- my_data %>% filter(ITENS.Codigo=="1122" & TipodeProduto=="Caldeira") %>%  
#   droplevels()
# my_filter1122$ITENS.Codigo <- droplevels(my_filter1122$ITENS.Codigo) #filtrando somente o item 1122 do tipo produto caldeira
# ggplot(my_filter1122,aes(x = Poder.Linear,y = SomaDeHorasApontadasUnitario, colour= Andrea_DEPARTAMENTO.Descricao)) + geom_count()+geom_smooth(method=lm,se=FALSE)

#### df mais_frequentes

mais_frequentes<-my_data %>% filter(ITENS.Codigo!="") %>%  
  group_by(ITENS.Codigo, Andrea_DEPARTAMENTO.Descricao) %>% #juntando os iguais
  count( sort = TRUE) %>% filter(n>=10)#%>% head(n=30) %>% droplevels()
mais_frequentes<-my_data %>%
  filter(paste(TipodeProduto)=="Caldeira" & ITENS.Codigo %in% mais_frequentes$ITENS.Codigo) %>%
  select(-Fonte,-OFFCod,-OFFEscopo,-SomaDeHorasApontadas,-ITENS.Descricao,
          Poder.Z,-Capacidade.TSS.D.,-Cap.Normal.Linear,-Cap.Normal.Z,
         -Evaporacao.T.H., -Evap.Normal.Linear,
         -Evap.Normal.Z,-Evaporacao.T.H.,-Concatena)

#renomeando as colunas no drf mais_frequentes

colnames(mais_frequentes)<-c("Depart","Qtd","Prod.Tipo","Prod.Cod","Prod.Nome","Item.Cod","Poder.Lin","Poder.Z","HorasUnit")
mais_frequentes<-mais_frequentes %>% droplevels()
levels(mais_frequentes$Prod.Tipo)
mais_frequentes<-mais_frequentes %>% select(-Prod.Cod,-Prod.Tipo)

mais_frequentes<-mais_frequentes[, c("HorasUnit", "Qtd", "Poder.Z", "Poder.Lin","Depart", "Prod.Nome", "Item.Cod")]

#criando colunas dummies para os dados categóricos no df mais_frequentes
dummies_frequentes<-dummy_cols(mais_frequentes)
dummies_frequentes<-dummies_frequentes %>% select(-Prod.Nome,-Item.Cod,-Depart)


### gráfico para o df mais_frequentes

#tudo no Jupyter
#gráfico no Jupyter
# #histograma
# ggplot(mais_frequentes, aes(HorasUnit, , fill = Prod.Nome)) + geom_histogram() + facet_wrap(~Prod.Nome) #Gprodutonome

#gráfico no Jupyter
# #histograma
# ggplot(mais_frequentes, aes(HorasUnit, , fill = Item.Cod)) + geom_histogram() + facet_wrap(~Item.Cod)   #Gitemcod


##filtro dos outros produtos

#tudo no Jupyter
#  my_coletor<- my_data %>% filter(ITENS.Codigo=="1129" | ITENS.Codigo=="1131" | ITENS.Codigo=="1151") %>%  #& TipodeProduto=="Caldeira") %>%  #
#    droplevels() 
#  my_coletor$ITENS.Codigo <- droplevels(my_coletor$ITENS.Codigo)
#  plot_ly(data=my_coletor,x=my_coletor$SomaDeHorasApontadasUnitario,color=my_coletor$ITENS.Codigo,type = "histogram",alpha = 0.6)%>%
#   layout(barmode = "overlay")

# my_tubos<- my_data %>% filter(ITENS.Codigo=="1113" | ITENS.Codigo=="1123" | ITENS.Codigo=="1132" | ITENS.Codigo=="1152" | ITENS.Codigo=="1600") %>%  
#   droplevels() 
# my_tubos$ITENS.Codigo <- droplevels(my_tubos$ITENS.Codigo)
# plot_ly(data=my_tubos,x=my_tubos$SomaDeHorasApontadasUnitario,color=my_tubos$ITENS.Codigo,type = "histogram",alpha = 0.6)%>%
#   layout(barmode = "overlay")

# my_linkpipe<- my_data %>% filter(ITENS.Codigo=="1134" | ITENS.Codigo=="1154") %>%  
#   droplevels() 
# my_linkpipe$ITENS.Codigo <- droplevels(my_linkpipe$ITENS.Codigo)
# plot_ly(data=my_linkpipe,x=my_linkpipe$SomaDeHorasApontadasUnitario,color=my_linkpipe$ITENS.Codigo,type = "histogram",alpha = 0.6)%>%
#   layout(barmode = "overlay")




#### correlação
# Create dummy variables:

my_dummies <-my_data%>%dplyr::select(-Concatena)

my_dummies <- dummy_cols(my_dummies,select_columns = c("Andrea_DEPARTAMENTO.Descricao",
                                                       "TipodeProduto","Produto.Nome"))
my_dummies<-my_dummies %>% dplyr::select(-OFFCod,-OFFEscopo,         
                                         -Andrea_DEPARTAMENTO.Descricao,-TipodeProduto,
                                         -Produto.Nome,-ITENS.Codigo,-ITENS.Descricao,-PRODUTOS.Codigo,-Fonte,
                                         -Cap.Normal.Z,-Capacidade.TSS.D.,-Poder.Z,-Cap.Normal.Linear,
                                         -Evaporacao.T.H.,-Evap.Normal.Z,-Evap.Normal.Linear) #,-Evaporacao.T.H)
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

#no Jupyter 
# ggcorr(my_dummies)#fun??o de visualiza??o para plotar uma matriz de correla??o
# ggcorr(my_dummies, nbreaks = 5) #nbreakrs: especifica quantas quebras devem estar 
# #contidas na escala de cores
# #ggcorrplot(my_dummies, hc.order = TRUE, type = "lower",
# #           lab = TRUE)

#no Jupyter
# correlacoesPODER <- cor(my_dummies[!is.na(my_dummies$Poder),])
# correlacoesNA <- cor(my_dummies[is.na(my_dummies$Poder),])
# ggcorrplot(correlacoesPODER) #matriz de correla??o
# ggcorrplot(correlacoesNA)
#print(dummies_frequentes)


###### correla??es para caldeiras mais frequentes

#no Jupyter
# correlacoesFREQUENTES <- cor(dummies_frequentes)#[!is.na(my_dummies$Poder),])
# ggcorr(dummies_frequentes, nbreaks = 5)
# correlacoesFREQUENTESNotNA <- cor(dummies_frequentes[!is.na(dummies_frequentes$Poder.Lin),])
# ggcorrplot(correlacoesFREQUENTESNotNA)
# ggcorr(dummies_frequentes[!is.na(dummies_frequentes$Poder.Lin),], nbreaks = 5)

# correlacoesFREQUENTESNotNA['Poder.Z',"HorasUnit"]
# correlacoesFREQUENTESNotNA['Poder.Lin',"HorasUnit"]


####### preparo dos dados para os modelos GLM ##

mais_frequentes<-mais_frequentes[!is.na(mais_frequentes$Poder.Z),]

medias<-mais_frequentes[mais_frequentes$Poder.Lin>0,] %>% group_by(Depart,Prod.Nome,Item.Cod) %>%
  summarise(Poder.Lin_=mean(Poder.Lin),Poder.Z_=mean(Poder.Z))

mais_frequentes<-left_join(mais_frequentes,medias)
mais_frequentes<-mais_frequentes %>% mutate(Poder.Lin=ifelse(Poder.Lin>0,Poder.Lin,Poder.Lin_),
                                            Poder.Z=ifelse(Poder.Lin>0,Poder.Z,Poder.Z_))
mais_frequentes$Poder.Z_<-NULL
mais_frequentes$Poder.Lin_<-NULL

mais_frequentes <- mais_frequentes %>% group_by(Depart, Prod.Nome, Item.Cod)
mais_frequentes$linha <- 1:nrow(mais_frequentes)
mais_frequentes$HorasUnit_log<-log(mais_frequentes$HorasUnit)
mais_frequentes$HorasUnit_sca<-scale(mais_frequentes$HorasUnit)

trainSample <- mais_frequentes %>% sample_frac(size=.6)
outSample <- mais_frequentes[!(mais_frequentes$linha %in% trainSample$linha),] # pode ignorar, usado para criar as amostras seguintes
Samples<-sample(seq(1,2),size=nrow(outSample),replace=TRUE,prob=c(0.5,0.5)) #pode ignorar tbm
testSample<-outSample[Samples==1,]
validSample<-outSample[Samples==2,]


