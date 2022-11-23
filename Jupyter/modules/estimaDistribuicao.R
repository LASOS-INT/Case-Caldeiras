### codigo criado por Iuri Santos (iurix_ms@hotmail.com / iuri.santos@tecgraf.puc-rio.br)
# # para saber como rodar ver arquivo exemplo-EstimaDistribuicao.R ###

# install.packages("magrittr") #, "%>%")
# install.packages("dplyr")
# install.packages("select") 
# install.packages("mutate")
# install.packages("sample_frac") #, keep_when = 
# install.packages("filter")
# install.packages("tidyr")
# install.packages("unnest")
# install.packages("tibble")
# install.packages("tibble")
# install.packages("broom")
# install.packages("tidy")
# install.packages("fitdistrplus")
# install.packages("fitdist")
# install.packages("denscomp")
# install.packages("cdfcomp")
# install.packages("qqcomp")
# install.packages("ppcomp")
# install.packages("graphics")
# install.packages("boxplot")
# install.packages("par")
# install.packages("stats")#,ks.test)
# install.packages("dgof")
# install.packages("ks.test")
# install.packages("actuar")
# install.packages("dllogis")
# install.packages("pllogis")
# install.packages("rllogis")
# install.packages("qllogis")
# install.packages("triangle")
# install.packages("dtriangle")
# install.packages("ptriangle")
# install.packages("qtriangle")
# install.packages("rtriangle")
# install.packages("goftest")
# install.packages("cvm.test")
# install.packages("ad.test")
# install.packages("grDevices")
# install.packages("pdf")
# install.packages("dev.off")
# install.packages("import")


import::from(magrittr, "%>%")
import::from(dplyr, select, mutate,sample_frac, keep_when = filter)
import::from(tidyr, unnest)
import::from(tibble, tibble)
import::from(broom, tidy)
import::from(fitdistrplus, fitdist,denscomp,cdfcomp,qqcomp,ppcomp)
import::from(graphics, boxplot,par)
import::from(stats)#,ks.test)
import::from(dgof,ks.test)
import::from(actuar,dllogis,pllogis,rllogis,qllogis)
import::from(triangle,dtriangle,ptriangle,qtriangle,rtriangle)
import::from(goftest,cvm.test,ad.test)
import::from(grDevices,pdf,dev.off)

# funcao para estimar e testar distribuicoes
# recebe 5 argumentos, sendo 2 obrigatorios (datasets e filtros)
# argumento 'datasets': � o conjunto de datasets a serem estimadas distribuicoes. pode ser apenas um ou um vetor de datasets
# argumento 'filtros' : se � um conjunto de datasets, este argumento deve ser TRUE, caso contrario deve ser FALSE
# argumento 'coluna' : � a coluna do dataset cuja a distribuicao sera estimada. o default � 'SomaDeHorasApontadasUnitario'
# argumento 'quant_amostragens' : quantidade de amostragens a serem feitas de cada dataset. por default ele � zero. quando � zero amostragens, ultiza-se o dataset inteiro.
# argumento 'fracao' : percentual do dataset a ser amostrado caso quant_amostragens for maior que 0. ultizamos um default de 60% do dataset a ser sorteado em cada amostragem
estima_distribuicao <- function(datasets,filtros, coluna='SomaDeHorasApontadasUnitario',quant_amostragens=0,fracao=.6){
  # verifica a quantidade de datasets de acordo com o booleano filtros
  if(filtros==TRUE){
    quant_datasets=length(datasets)
  } else{
    quant_datasets=1
  }
  # i sao os datasets
  # j sao os com ou sem outliers
  # k sao os samples (amostragens)
  for (i in 1:quant_datasets){
    for (j in 1:2){
      for (k in 0:quant_amostragens){
        print(paste0("i",i,"j",j,"k",k))
        # seleciona os datasets
        if (filtros==TRUE){
          filtrado=datasets[[i]]
        } else{
          filtrado=datasets
        }
        grupoatividade=i
        # com ou sem outlier (a eliminacao � so depois)
        if (j==1){
          outlier="Com"
        } else{
          outlier="Sem"
        }
        # retira amostragens dos datasets
        if (k>0){
          filtrado = dplyr::sample_frac(filtrado,size=fracao)
        }
        # nome da instancia
        NomeArquivo = paste("DURACAO -",i,"-",outlier,"Outlier -",k) #Nome do arquivo com identifica��o do tipo de teste
        # foi necessario mudar a dimensao para os testes, dependendo do dado, vale comentar essa parte
        filtrado[coluna]<-filtrado[coluna]/1000
        filtrado2<-filtrado[coluna][[1]]
        
          
        #######   DETEC��O DE OUTLIERS #####
        print("outliers")
        #graphics::boxplot(filtrado2, main="DURACAO",xlab="DURACAOX", ylab="DURACAOY")
        outliers<- graphics::boxplot(filtrado2, main="Duracao",xlab="DURACAOX", ylab="DURACAOY")$out
        #graphics::boxplot(filtrado[coluna], main="DURACAO",xlab="DURACAOX", ylab="DURACAOY")$conf
        #graphics::boxplot(filtrado[coluna], main="DURACAO",xlab="DURACAOX", ylab="DURACAOY")$stats
        LB<-graphics::boxplot(filtrado2, main="Duracao",xlab="DURACAOX", ylab="DURACAOY")$stats[1,1]
        UB<-graphics::boxplot(filtrado2, main="Duracao",xlab="DURACAOX", ylab="DURACAOY")$stats[5,1]
        MED<-graphics::boxplot(filtrado2, main="Duracao",xlab="DURACAOX", ylab="DURACAOY")$stats[3,1]
        #######   FIM DETECCAO DE OUTLIER #####
        if (j==2){
          #######   ELIMINA��O DE OUTLIERS #####
          filtrado<- filtrado2[filtrado2 >= LB]
          filtrado<- filtrado2[filtrado2 <= UB]
          #######   FIM DE ELIMINA��O DE OUTLIERS #####
        }
        ############## ESTIMA DISTRIBUICOES ##########
        print("estimacao")
        x = as.numeric(filtrado2)
        summary(x)
        minT<-min(x)
        maxT<-max(x)
        #print(x)
        # estima par�metros das distribui��es (fit)
        fit_w   <- fitdistrplus::fitdist(x, "weibull")
        fit_g   <- fitdistrplus::fitdist(x, "gamma")
        #print(typeof(x))
        fit_c   <- fitdistrplus::fitdist(x, "cauchy")
        fit_ln  <- fitdistrplus::fitdist(x, "lnorm")
        fit_ll  <- fitdistrplus::fitdist(x, "llogis")
        fit_ex  <- fitdistrplus::fitdist(x, "exp")
        fit_n  <- fitdistrplus::fitdist(x, "norm")
        #fit_b  <- fitdistrplus::fitdist(x, "binom",fix.arg=list(size=5), start=list(prob=0.2))
        fit_t <- fitdistrplus::fitdist(x,"triangle",method="mge",start = list(a=minT, b=maxT, c=MED),gof="CvM")
        
        #fit_ll2 <- fitdistrplus::fitdist(x, "llogis", fix.arg = list(shape=median(x)))
        #fit_n  <- fitdistrplus::fitdist(x, "normal")
        # fit_t  <- fitdistrplus::fitdist(x, "triangle", method = "qme", probs = c(0.1, 0.5, 0.9), start = list(a = 0, b = 49, c = 0.02) )
        
        ############## FIM DA ESTIMA��O DAS DISTRIBUICOES ##########
        print("graficos")
        ###### VISUALIZA DISTRIBUICOES ############
        # gera gr?fico com informa??es das distribui??es estimadas
        #pdf(paste0("C:/Users/",usuario,"/Desktop/TestsPlots_", NomeArquivo,".pdf"))
        grDevices::pdf(paste0(getwd(),"/",NomeArquivo,".pdf"))
        graphics::par(mfrow=c(2,2))
        plot.legend <- c("weibull","gamma", "lognormal","llogis", "exponential", "normal", "triang")
        plot.dis = list(fit_w, fit_g, fit_ln, fit_ll, fit_ex, fit_n, fit_t)
        fitdistrplus::denscomp(plot.dis, legendtext = plot.legend)
        fitdistrplus::cdfcomp (plot.dis, legendtext = plot.legend)
        fitdistrplus::qqcomp  (plot.dis, legendtext = plot.legend)
        fitdistrplus::ppcomp  (plot.dis, legendtext = plot.legend)
        grDevices::dev.off()
        
        ###### FIM DA VISUALIZA��O DAS DISTRIBUICOES ############
        print("testes")
        ###### testes das distribui�oes estimadas ######
        # gera informa��es de goodness-of-fit
        #gofstat(plot.dis, fitnames = plot.legend)
        distibuicao="plnorm"
        distribuicaoFuncao=stats::plnorm
        #typeof(stats::plnorm)
        ksT<-dgof::ks.test(x, distribuicaoFuncao, meanlog=fit_ln$estimate[1], sdlog = fit_ln$estimate[2])
        if (i==1 && j==1 && k==0){
          teste_aderencia_df<-tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                     method=c(ksT$method),hypothesis=c(ksT$alternative),statistics=c(ksT$statistic),pValue=c(ksT$p.value),
                                     par1=c(fit_ln$estimate[1]),par2=c(fit_ln$estimate[2]),par3=c(NA))
        }else{
          teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                              method=c(ksT$method),hypothesis=c(ksT$alternative),statistics=c(ksT$statistic),pValue=c(ksT$p.value),
                                    par1=c(fit_ln$estimate[1]),par2=c(fit_ln$estimate[2]),par3=c(NA)))
        }
        cvmT<-goftest::cvm.test(x, distribuicaoFuncao, meanlog=fit_ln$estimate[1], sdlog = fit_ln$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                      method=c(cvmT$method[1]),hypothesis=c(cvmT$method[2]),statistics=c(cvmT$statistic),pValue=c(cvmT$p.value),
                                      par1=c(fit_ln$estimate[1]),par2=c(fit_ln$estimate[2]),par3=c(NA)))
        
        adT<-goftest::ad.test(x, distribuicaoFuncao, meanlog=fit_ln$estimate[1], sdlog = fit_ln$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(adT$method[1]),hypothesis=c(adT$method[2]),statistics=c(adT$statistic),pValue=c(adT$p.value),
                                                            par1=c(fit_ln$estimate[1]),par2=c(fit_ln$estimate[2]),par3=c(NA)))
        
        # pweibull
        distibuicao="pweibull"
        distribuicaoFuncao=stats::pweibull
        ksT<-dgof::ks.test(x, distribuicaoFuncao, shape=fit_w$estimate[1], scale = fit_w$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(ksT$method),hypothesis=c(ksT$alternative),statistics=c(ksT$statistic),pValue=c(ksT$p.value),
                                                            par1=c(fit_w$estimate[1]),par2=c(fit_w$estimate[2]),par3=c(NA)))
        
        cvmT<-goftest::cvm.test(x, distribuicaoFuncao, shape=fit_w$estimate[1], scale = fit_w$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(cvmT$method[1]),hypothesis=c(cvmT$method[2]),statistics=c(cvmT$statistic),pValue=c(cvmT$p.value),
                                                            par1=c(fit_w$estimate[1]),par2=c(fit_w$estimate[2]),par3=c(NA)))
        
        adT<-goftest::ad.test(x, distribuicaoFuncao, shape=fit_w$estimate[1], scale = fit_w$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(adT$method[1]),hypothesis=c(adT$method[2]),statistics=c(adT$statistic),pValue=c(adT$p.value),
                                                            par1=c(fit_w$estimate[1]),par2=c(fit_w$estimate[2]),par3=c(NA)))
        
        # pllogis
        distibuicao="pllogis"
        distribuicaoFuncao=actuar::pllogis
        ksT<-dgof::ks.test(x, distribuicaoFuncao, shape=fit_ll$estimate[1], scale = fit_ll$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(ksT$method),hypothesis=c(ksT$alternative),statistics=c(ksT$statistic),pValue=c(ksT$p.value),
                                                            par1=c(fit_ll$estimate[1]),par2=c(fit_ll$estimate[2]),par3=c(NA)))
        
        cvmT<-goftest::cvm.test(x, distribuicaoFuncao, shape=fit_ll$estimate[1], scale = fit_ll$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(cvmT$method[1]),hypothesis=c(cvmT$method[2]),statistics=c(cvmT$statistic),pValue=c(cvmT$p.value),
                                                            par1=c(fit_ll$estimate[1]),par2=c(fit_ll$estimate[2]),par3=c(NA)))
        
        adT<-goftest::ad.test(x, distribuicaoFuncao, shape=fit_ll$estimate[1], scale = fit_ll$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(adT$method[1]),hypothesis=c(adT$method[2]),statistics=c(adT$statistic),pValue=c(adT$p.value),
                                                            par1=c(fit_ll$estimate[1]),par2=c(fit_ll$estimate[2]),par3=c(NA)))
        
        # pgamma
        distibuicao="pgamma"
        distribuicaoFuncao=stats::pgamma
        ksT<-dgof::ks.test(x, distribuicaoFuncao, shape=fit_g$estimate[1], rate = fit_g$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(ksT$method),hypothesis=c(ksT$alternative),statistics=c(ksT$statistic),pValue=c(ksT$p.value),
                                                            par1=c(fit_g$estimate[1]),par2=c(fit_g$estimate[2]),par3=c(NA)))
        
        cvmT<-goftest::cvm.test(x, distribuicaoFuncao, shape=fit_g$estimate[1], rate = fit_g$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(cvmT$method[1]),hypothesis=c(cvmT$method[2]),statistics=c(cvmT$statistic),pValue=c(cvmT$p.value),
                                                            par1=c(fit_g$estimate[1]),par2=c(fit_g$estimate[2]),par3=c(NA)))
        
        adT<-goftest::ad.test(x, distribuicaoFuncao, shape=fit_g$estimate[1], rate = fit_g$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(adT$method[1]),hypothesis=c(adT$method[2]),statistics=c(adT$statistic),pValue=c(adT$p.value),
                                                            par1=c(fit_g$estimate[1]),par2=c(fit_g$estimate[2]),par3=c(NA)))
        
        # pcauchy
        #fit_c   <- fitdistrplus::fitdist(x/100, "cauchy") #sometimes needed
        distibuicao="pcauchy"
        distribuicaoFuncao=stats::pcauchy
        ksT<-dgof::ks.test(x, distribuicaoFuncao, location=fit_c$estimate[1], scale = fit_c$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(ksT$method),hypothesis=c(ksT$alternative),statistics=c(ksT$statistic),pValue=c(ksT$p.value),
                                                            par1=c(fit_c$estimate[1]),par2=c(fit_c$estimate[2]),par3=c(NA)))
        
        cvmT<-goftest::cvm.test(x, distribuicaoFuncao, location=fit_c$estimate[1], scale = fit_c$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(cvmT$method[1]),hypothesis=c(cvmT$method[2]),statistics=c(cvmT$statistic),pValue=c(cvmT$p.value),
                                                            par1=c(fit_c$estimate[1]),par2=c(fit_c$estimate[2]),par3=c(NA)))
        
        adT<-goftest::ad.test(x, distribuicaoFuncao, location=fit_c$estimate[1], scale = fit_c$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(adT$method[1]),hypothesis=c(adT$method[2]),statistics=c(adT$statistic),pValue=c(adT$p.value),
                                                            par1=c(fit_c$estimate[1]),par2=c(fit_c$estimate[2]),par3=c(NA)))
        
        # pexp
        distibuicao="pexp"
        distribuicaoFuncao=stats::pexp
        ksT<-dgof::ks.test(x, distribuicaoFuncao, rate=fit_ex$estimate[1])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(ksT$method),hypothesis=c(ksT$alternative),statistics=c(ksT$statistic),pValue=c(ksT$p.value),
                                                            par1=c(fit_ex$estimate[1]),par2=c(NA),par3=c(NA)))
        
        cvmT<-goftest::cvm.test(x, distribuicaoFuncao, rate=fit_ex$estimate[1])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(cvmT$method[1]),hypothesis=c(cvmT$method[2]),statistics=c(cvmT$statistic),pValue=c(cvmT$p.value),
                                                            par1=c(fit_ex$estimate[1]),par2=c(NA),par3=c(NA)))
        
        adT<-goftest::ad.test(x, distribuicaoFuncao, rate=fit_ex$estimate[1])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(adT$method[1]),hypothesis=c(adT$method[2]),statistics=c(adT$statistic),pValue=c(adT$p.value),
                                                            par1=c(fit_ex$estimate[1]),par2=c(NA),par3=c(NA)))
        
        # pnorm
        distibuicao="pnorm"
        distribuicaoFuncao=stats::pnorm
        ksT<-dgof::ks.test(x, distribuicaoFuncao, mean=fit_n$estimate[1], sd = fit_n$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(ksT$method),hypothesis=c(ksT$alternative),statistics=c(ksT$statistic),pValue=c(ksT$p.value),
                                                            par1=c(fit_n$estimate[1]),par2=c(fit_n$estimate[2]),par3=c(NA)))
        
        cvmT<-goftest::cvm.test(x, distribuicaoFuncao, mean=fit_n$estimate[1], sd = fit_n$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(cvmT$method[1]),hypothesis=c(cvmT$method[2]),statistics=c(cvmT$statistic),pValue=c(cvmT$p.value),
                                                            par1=c(fit_n$estimate[1]),par2=c(fit_n$estimate[2]),par3=c(NA)))
        
        adT<-goftest::ad.test(x, distribuicaoFuncao, mean=fit_n$estimate[1], sd = fit_n$estimate[2])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(adT$method[1]),hypothesis=c(adT$method[2]),statistics=c(adT$statistic),pValue=c(adT$p.value),
                                                            par1=c(fit_n$estimate[1]),par2=c(fit_n$estimate[2]),par3=c(NA)))
        
        ## ptriangle
        distibuicao="ptriangle"
        distribuicaoFuncao=triangle ::ptriangle
        ksT<-dgof::ks.test(x, distribuicaoFuncao, a=fit_t$estimate[1], b = fit_t$estimate[2], c = fit_t$estimate[3])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(ksT$method),hypothesis=c(ksT$alternative),statistics=c(ksT$statistic),pValue=c(ksT$p.value),
                                                            par1=c(fit_t$estimate[1]),par2=c(fit_t$estimate[2]),par3=c(fit_t$estimate[3])))
        
        cvmT<-goftest::cvm.test(x, distribuicaoFuncao, a=fit_t$estimate[1], b = fit_t$estimate[2], c = fit_t$estimate[3])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(cvmT$method[1]),hypothesis=c(cvmT$method[2]),statistics=c(cvmT$statistic),pValue=c(cvmT$p.value),
                                                            par1=c(fit_t$estimate[1]),par2=c(fit_t$estimate[2]),par3=c(fit_t$estimate[3])))
        
        adT<-goftest::ad.test(x, distribuicaoFuncao, a=fit_t$estimate[1], b = fit_t$estimate[2], c = fit_t$estimate[3])
        teste_aderencia_df<-rbind(teste_aderencia_df,tibble::tibble(dataset=c(i),j=c(j),k=c(k),dado=c(NomeArquivo),distribuicao=c(distibuicao),
                                                               method=c(adT$method[1]),hypothesis=c(adT$method[2]),statistics=c(adT$statistic),pValue=c(adT$p.value),
                                                            par1=c(fit_t$estimate[1]),par2=c(fit_t$estimate[2]),par3=c(fit_t$estimate[3])))
        
        ###### fim dos testes das distribui�oes estimadas ######
      }
    }
  }
  return(teste_aderencia_df)
}

############# codigo de exemplo #############
# # gerando amostras do exemplo
# alpha = 10
# theta = 150 / 60
# # Creates dataframe
# df1 = tibble::tibble(
#   obs = 1:1000,
#   y = rgamma(n = 1000, shape = alpha, scale = theta)*100,
#   x = rweibull(n = 1000, shape = alpha, scale = theta*10)*100,
#   SomaDeHorasApontadasUnitario = append(rweibull(n = 500, shape = alpha, scale = theta*10),
#                                         rgamma(n = 500, shape = alpha, scale = theta))*100
# )
# # Creates another dataframe
# df2 = tibble::tibble(
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
# resultadoTeste=estima_distribuicao(dadosTestes,TRUE, coluna='x')
# # vendo resultado do exemplo
# resultadoTeste
# #segundo teste
# resultadoTeste=estima_distribuicao(dadosTestes,TRUE)
# # vendo resultado do exemplo
# resultadoTeste
# #terceiro teste
# resultadoTeste=estima_distribuicao(dadosTestes,TRUE,quant_amostragens=2)
# # vendo resultado do exemplo
# resultadoTeste
# 
# #quarto teste
# resultadoTeste=estima_distribuicao(df1,FALSE,quant_amostragens=1)
# # vendo resultado do exemplo
# resultadoTeste
# 
# 
# # para chamar funcao, recomendo usar como modelo
# # https://cran.r-project.org/web/packages/modules/vignettes/modulesInR.html
# # install.packages("modules")
# # m <- modules::use("estimaDistribuicao.R")
# # m$estima_distribuicao(df1,FALSE)