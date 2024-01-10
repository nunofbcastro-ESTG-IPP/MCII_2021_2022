# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
#     Disciplina: Matematica Computacional II
#     Ano Letivo: 2021/2022
# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
# Bruno Dylan Pinto Ferreira - 8200586
# Gon�alo Andr� Fontes Oliveira - 8200595
# Jorge Miguel Fernandes Correia - 8200592
# Nuno de Figueiredo Brito e Castro - 8200591
# Last update: 22/01/2022
# - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 
# Trabalho Pr�tico II
# - - - - - - - - - - - - - - - - - - - - - - - - - - 

library(readxl)
setwd("C:/Users/bruno/Desktop/Universidade_2Ano/MCI/TrabalhoPraticoII/Scripts/")
dados <- read_excel("MC_II_Recolhas1_2_3_4.xlsx", 
                    col_types = c("numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric"), col_names = FALSE, skip = 2);
colnames(dados) = c("Recolha","Grupo","nr_total_linhas","nr_classe","nr_linhas_pClasse","nr_testes_pClasse","nr_testes","nr_teste_falharam","nr_testes_falhados_pClasse","code_coverage","code_coverage_pClasse","Cxty","Cxty_pClasse ", "percentagem_docomentacao","docomentacao_falta","duplicacao_codigo","nr_LOC_comentadas_pClasse");

View(dados)

# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
#     M�trica 1
# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
#Tabela de frequ�ncias para o n�mero de linhas comentadas

dadosMetrica1 = dados[!with(dados,is.na(nr_testes_falhados_pClasse) & is.na(nr_LOC_comentadas_pClasse) | nr_testes_pClasse == 0 | is.na(nr_testes_pClasse)) ,]
dadosMetrica1 <- subset( dadosMetrica1, select = c(nr_testes_falhados_pClasse, 
                                         nr_LOC_comentadas_pClasse))

dadosMetrica1[is.na(dadosMetrica1)] <- 0
View(dadosMetrica1);

attach(dadosMetrica1)

nobs <- nrow(dadosMetrica1) # n�mero de observa��es = dimensao da amostra
k <- 1+round(log(nobs)/log(2))

amplitudeComentarios <- diff(range(nr_LOC_comentadas_pClasse))/k
amplitudeComentarios <- ceiling(amplitudeComentarios)
brkComentatrios  <- seq(min(nr_LOC_comentadas_pClasse),
                        max(nr_LOC_comentadas_pClasse)+amplitudeComentarios, 
                        amplitudeComentarios)

classesComentarios <- c()
for (ii in c(1:(length(brkComentatrios)-1))){
  classesComentarios <- c(classesComentarios,
                          paste("[",brkComentatrios[ii],", ",brkComentatrios[ii+1],"[",sep = ""))
}

#Tabela de frequ�ncias paras as duas vari�veis
tabsComentarios <- table(cut(nr_LOC_comentadas_pClasse, breaks = brkComentatrios, right=F, labels = classesComentarios))
TabFreqComentarios <- cbind(ni = tabsComentarios,
                            Ni = cumsum(tabsComentarios),
                            fi = prop.table(tabsComentarios),
                            Fi = cumsum(prop.table(tabsComentarios))
)

TabFreqComentarios
library(xtable)
print(xtable(TabFreqComentarios)) # gera o c�digo LaTeX da Tabela de Frequ�ncias

#------------------------------------------------
#Tabela de frequ�ncias para o n�mero de testes falhados

amplitudeTestes <- diff(range(nr_testes_falhados_pClasse))/k
amplitudeTestes <- ceiling(amplitudeTestes)
brkTestes  <- seq(min(nr_testes_falhados_pClasse),
                  max(nr_testes_falhados_pClasse)+amplitudeTestes, 
                  amplitudeTestes)

classesTestes <- c()
for (ii in c(1:(length(brkTestes)-1))){
  classesTestes <- c(classesTestes,
                     paste("[",brkTestes[ii],", ",brkTestes[ii+1],"[",sep = ""))
}

#Tabela de frequ�ncias paras as duas vari�veis
tabsTestes <- table(cut(nr_testes_falhados_pClasse, breaks = brkTestes, right=F, labels = classesTestes))
TabFreqTestes <- cbind(ni = tabsTestes,
                       Ni = cumsum(tabsTestes),
                       fi = prop.table(tabsTestes),
                       Fi = cumsum(prop.table(tabsTestes))
)

TabFreqTestes
library(xtable)
print(xtable(TabFreqTestes, type="latex")) # gera o c�digo LaTeX da Tabela de Frequ�ncias

#-------------------------------------------------

summary (dadosMetrica1)

quantile (nr_testes_falhados_pClasse, type=2)
quantile (nr_LOC_comentadas_pClasse, type=2)

#Amplitude da amostra
diff(range(nr_testes_falhados_pClasse))
diff(range(nr_LOC_comentadas_pClasse))

#Amplitude interquartil
IQR(nr_testes_falhados_pClasse)
IQR(nr_LOC_comentadas_pClasse)

#Vari�ncia 
var(nr_testes_falhados_pClasse)
var(nr_LOC_comentadas_pClasse)

#Desvio padr�o
sd(nr_testes_falhados_pClasse)
sd(nr_LOC_comentadas_pClasse)

#Coeficiente de variacao
cvTestes <- sd(nr_testes_falhados_pClasse) / mean(nr_testes_falhados_pClasse) * 100
cvComentarios <- sd(nr_LOC_comentadas_pClasse) / mean(nr_LOC_comentadas_pClasse) * 100

cvTestes
cvComentarios

#Devio absoluto m�dio
mad(nr_testes_falhados_pClasse)
mad(nr_LOC_comentadas_pClasse)

#An�lise da assimetria e curtose
library(e1071) # assimetria
skewness(nr_testes_falhados_pClasse)
skewness(nr_LOC_comentadas_pClasse)

#curtose
kurtosis(nr_testes_falhados_pClasse)
kurtosis(nr_LOC_comentadas_pClasse)

#Teste de correla��o
cor.test(nr_LOC_comentadas_pClasse, nr_testes_falhados_pClasse, method= "spearman", exact=FALSE)

detach(dadosMetrica1)

# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
#     M�trica 2
# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

dadosMetrica2 = dados[!with(dados, is.na(nr_LOC_comentadas_pClasse) ) ,]

#C�lculos para a recolha1
dadosMetrica2Recolha1 <- subset( dadosMetrica2, Recolha == 1, select = c(Recolha, 
                                                   nr_LOC_comentadas_pClasse, 
                                                   nr_testes_falhados_pClasse))

dadosMetrica2Recolha1[is.na(dadosMetrica2Recolha1)] <- 0
View(dadosMetrica2Recolha1);

attach(dadosMetrica2Recolha1)

summary(dadosMetrica2Recolha1)

#Estudo da simetria 
skewness(nr_LOC_comentadas_pClasse)
skewness(nr_testes_falhados_pClasse)

#Estudo da curtose
kurtosis(nr_LOC_comentadas_pClasse)
kurtosis(nr_testes_falhados_pClasse)

quantile (nr_testes_falhados_pClasse, type=2)
quantile (nr_LOC_comentadas_pClasse, type=2)

#Amplitude da amostra
diff(range(nr_testes_falhados_pClasse))
diff(range(nr_LOC_comentadas_pClasse))

#Amplitude interquartil
IQR(nr_testes_falhados_pClasse)
IQR(nr_LOC_comentadas_pClasse)

#Vari�ncia 
var(nr_testes_falhados_pClasse)
var(nr_LOC_comentadas_pClasse)

#Desvio padr�o
sd(nr_testes_falhados_pClasse)
sd(nr_LOC_comentadas_pClasse)

#Coeficiente de variacao
cvTestes <- sd(nr_testes_falhados_pClasse) / mean(nr_testes_falhados_pClasse) * 100
cvComentarios <- sd(nr_LOC_comentadas_pClasse) / mean(nr_LOC_comentadas_pClasse) * 100

cvTestes
cvComentarios

#Devio absoluto m�dio
mad(nr_testes_falhados_pClasse)
mad(nr_LOC_comentadas_pClasse)

detach(dadosMetrica2Recolha1)

#C�lculos para a recolha2
dadosMetrica2Recolha2 <- subset( dadosMetrica2, Recolha == 2, select = c(Recolha, 
                                                                         nr_LOC_comentadas_pClasse, 
                                                                         nr_testes_falhados_pClasse))

dadosMetrica2Recolha2[is.na(dadosMetrica2Recolha2)] <- 0
View(dadosMetrica2Recolha2);

attach(dadosMetrica2Recolha2)

summary(dadosMetrica2Recolha2)

#Estudo da simetria
skewness(nr_LOC_comentadas_pClasse)
skewness(nr_testes_falhados_pClasse)

#Estudo da curtose
kurtosis(nr_LOC_comentadas_pClasse)
kurtosis(nr_testes_falhados_pClasse)

quantile (nr_testes_falhados_pClasse, type=2)
quantile (nr_LOC_comentadas_pClasse, type=2)

#Amplitude da amostra
diff(range(nr_testes_falhados_pClasse))
diff(range(nr_LOC_comentadas_pClasse))

#Amplitude interquartil
IQR(nr_testes_falhados_pClasse)
IQR(nr_LOC_comentadas_pClasse)

#Vari�ncia 
var(nr_testes_falhados_pClasse)
var(nr_LOC_comentadas_pClasse)

#Desvio padr�o
sd(nr_testes_falhados_pClasse)
sd(nr_LOC_comentadas_pClasse)

#Coeficiente de variacao
cvTestes <- sd(nr_testes_falhados_pClasse) / mean(nr_testes_falhados_pClasse) * 100
cvComentarios <- sd(nr_LOC_comentadas_pClasse) / mean(nr_LOC_comentadas_pClasse) * 100

cvTestes
cvComentarios

#Devio absoluto m�dio
mad(nr_testes_falhados_pClasse)
mad(nr_LOC_comentadas_pClasse)

#teste de hip�teses para o n�mero de linhas comentadas (Recolha 2 - 1)
t.test(nr_LOC_comentadas_pClasse, 
       alternative="less",
       mu = 12.57, 
       conf.level = 0.90)

t.test(nr_LOC_comentadas_pClasse, 
       alternative="less",
       mu = 12.57, 
       conf.level = 0.95)

t.test(nr_LOC_comentadas_pClasse, 
       alternative="less",
       mu = 12.57, 
       conf.level = 0.99)

#teste de hip�teses para o n�mero de testes falhados (Recolha 2 - 1)
t.test(nr_testes_falhados_pClasse, 
       alternative="less",
       mu = 0, 
       conf.level = 0.90)

t.test(nr_testes_falhados_pClasse, 
       alternative="less",
       mu = 0, 
       conf.level = 0.95)

t.test(nr_testes_falhados_pClasse, 
       alternative="less",
       mu = 0, 
       conf.level = 0.99)

detach(dadosMetrica2Recolha2)

#C�lculos para a recolha3
dadosMetrica2Recolha3 <- subset( dadosMetrica2, Recolha == 3, select = c(Recolha, 
                                                                         nr_LOC_comentadas_pClasse, 
                                                                         nr_testes_falhados_pClasse))

dadosMetrica2Recolha3[is.na(dadosMetrica2Recolha3)] <- 0
View(dadosMetrica2Recolha3);

attach(dadosMetrica2Recolha3)

summary(dadosMetrica2Recolha3)

skewness(nr_LOC_comentadas_pClasse)
skewness(nr_testes_falhados_pClasse)

kurtosis(nr_LOC_comentadas_pClasse)
kurtosis(nr_testes_falhados_pClasse)

quantile (nr_testes_falhados_pClasse, type=2)
quantile (nr_LOC_comentadas_pClasse, type=2)

#Amplitude da amostra
diff(range(nr_testes_falhados_pClasse))
diff(range(nr_LOC_comentadas_pClasse))

#Amplitude interquartil
IQR(nr_testes_falhados_pClasse)
IQR(nr_LOC_comentadas_pClasse)

#Vari�ncia 
var(nr_testes_falhados_pClasse)
var(nr_LOC_comentadas_pClasse)

#Desvio padr�o
sd(nr_testes_falhados_pClasse)
sd(nr_LOC_comentadas_pClasse)

#Coeficiente de variacao
cvTestes <- sd(nr_testes_falhados_pClasse) / mean(nr_testes_falhados_pClasse) * 100
cvComentarios <- sd(nr_LOC_comentadas_pClasse) / mean(nr_LOC_comentadas_pClasse) * 100

cvTestes
cvComentarios

#Devio absoluto m�dio
mad(nr_testes_falhados_pClasse)
mad(nr_LOC_comentadas_pClasse)

#teste de hip�teses para o n�mero de linhas comentadas (Recolha 3 - 2)
t.test(nr_LOC_comentadas_pClasse, 
       alternative="less",
       mu = 7.108, 
       conf.level = 0.90)

t.test(nr_LOC_comentadas_pClasse, 
       alternative="less",
       mu = 7.108, 
       conf.level = 0.95)

t.test(nr_LOC_comentadas_pClasse, 
       alternative="less",
       mu = 7.108,  
       conf.level = 0.99)


#teste de hip�teses para o n�mero de testes falhados (Recolha 3 - 2)
t.test(nr_testes_falhados_pClasse, 
       alternative="less",
       mu = 0.6216, 
       conf.level = 0.90)

t.test(nr_testes_falhados_pClasse, 
       alternative="less",
       mu = 0.6216, 
       conf.level = 0.95)

t.test(nr_testes_falhados_pClasse, 
       alternative="less",
       mu = 0.6216,  
       conf.level = 0.99)

detach(dadosMetrica2Recolha3)

#C�lculos para a recolha4
dadosMetrica2Recolha4 <- subset( dadosMetrica2, Recolha == 4, select = c(Recolha, 
                                                                         nr_LOC_comentadas_pClasse, 
                                                                         nr_testes_falhados_pClasse))

dadosMetrica2Recolha4[is.na(dadosMetrica2Recolha4)] <- 0
View(dadosMetrica2Recolha4);

attach(dadosMetrica2Recolha4)

summary(dadosMetrica2Recolha4)

skewness(nr_LOC_comentadas_pClasse)
skewness(nr_testes_falhados_pClasse)

kurtosis(nr_LOC_comentadas_pClasse)
kurtosis(nr_testes_falhados_pClasse)

quantile (nr_testes_falhados_pClasse, type=2)
quantile (nr_LOC_comentadas_pClasse, type=2)

#Amplitude da amostra
diff(range(nr_testes_falhados_pClasse))
diff(range(nr_LOC_comentadas_pClasse))

#Amplitude interquartil
IQR(nr_testes_falhados_pClasse)
IQR(nr_LOC_comentadas_pClasse)

#Vari�ncia 
var(nr_testes_falhados_pClasse)
var(nr_LOC_comentadas_pClasse)

#Desvio padr�o
sd(nr_testes_falhados_pClasse)
sd(nr_LOC_comentadas_pClasse)

#Coeficiente de variacao
cvTestes <- sd(nr_testes_falhados_pClasse) / mean(nr_testes_falhados_pClasse) * 100
cvComentarios <- sd(nr_LOC_comentadas_pClasse) / mean(nr_LOC_comentadas_pClasse) * 100

cvTestes
cvComentarios

#Devio absoluto m�dio
mad(nr_testes_falhados_pClasse)
mad(nr_LOC_comentadas_pClasse)

#teste de hip�teses para o n�mero de linhas comentadas (Recolha 4 - 3)
t.test(nr_LOC_comentadas_pClasse, 
       alternative="less",
       mu = 37.56, 
       conf.level = 0.90)

t.test(nr_LOC_comentadas_pClasse, 
       alternative="less",
       mu = 37.56, 
       conf.level = 0.95)

t.test(nr_LOC_comentadas_pClasse, 
       alternative="less",
       mu = 37.56,  
       conf.level = 0.99)


#teste de hip�teses para o n�mero de testes falhados (Recolha 4)
t.test(nr_testes_falhados_pClasse, 
       alternative="less",
       mu = 0.04951, 
       conf.level = 0.90)

t.test(nr_testes_falhados_pClasse, 
       alternative="less",
       mu = 0.04951, 
       conf.level = 0.95)

t.test(nr_testes_falhados_pClasse, 
       alternative="less",
       mu = 0.04951,  
       conf.level = 0.99)

detach(dadosMetrica2Recolha4)

# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
#     M�trica 3
# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
#Tabela de frequ�ncias para o n�mero de linhas comentadas

dadosMetrica3 = dados[!with(dados,is.na(nr_total_linhas) | is.na(Cxty)) ,]
dadosMetrica3 <- subset( dadosMetrica3, select = c(nr_total_linhas, Cxty))



View(dadosMetrica3);

attach(dadosMetrica3)

nobs <- nrow(dadosMetrica3) # n�mero de observa��es = dimensao da amostra
nobs
k <- 1+round(log(nobs)/log(2))

amplitudeNrTotalLinhas <- diff(range(nr_total_linhas))/k
amplitudeNrTotalLinhas <- ceiling(amplitudeNrTotalLinhas)
brkNrTotalLinhas  <- seq(min(nr_total_linhas),
                         max(nr_total_linhas)+amplitudeNrTotalLinhas, 
                         amplitudeNrTotalLinhas)

classesNrTotalLinhas <- c()
for (ii in c(1:(length(brkNrTotalLinhas)-1))){
  classesNrTotalLinhas <- c(classesNrTotalLinhas,
                            paste("[",brkNrTotalLinhas[ii],", ",brkNrTotalLinhas[ii+1],"[",sep = ""))
}

#Tabela de frequ�ncias paras as duas vari�veis
tabsNrTotalLinhas <- table(cut(nr_total_linhas, breaks = brkNrTotalLinhas, right=F, labels = classesNrTotalLinhas))
TabFreqNrTotalLinhas <- cbind(ni = tabsNrTotalLinhas,
                              Ni = cumsum(tabsNrTotalLinhas),
                              fi = prop.table(tabsNrTotalLinhas),
                              Fi = cumsum(prop.table(tabsNrTotalLinhas))
)

TabFreqNrTotalLinhas
library(xtable)
print(xtable(TabFreqNrTotalLinhas)) # gera o c�digo LaTeX da Tabela de Frequ�ncias

#------------------------------------------------
#Tabela de frequ�ncias para o n�mero de testes falhados

amplitudeCxty <- diff(range(Cxty))/k
amplitudeCxty <- ceiling(amplitudeCxty)
brkCxty  <- seq(min(Cxty),
                max(Cxty)+amplitudeCxty, 
                amplitudeCxty)

classesCxty <- c()
for (ii in c(1:(length(brkCxty)-1))){
  classesCxty <- c(classesCxty,
                   paste("[",brkCxty[ii],", ",brkCxty[ii+1],"[",sep = ""))
}

#Tabela de frequ�ncias paras as duas vari�veis
tabsCxty <- table(cut(Cxty, breaks = brkCxty, right=F, labels = classesCxty))
TabFreqCxty <- cbind(ni = tabsCxty,
                     Ni = cumsum(tabsCxty),
                     fi = prop.table(tabsCxty),
                     Fi = cumsum(prop.table(tabsCxty))
)

TabFreqCxty
library(xtable)
print(xtable(TabFreqCxty, type="latex")) # gera o c�digo LaTeX da Tabela de Frequ�ncias

#-------------------------------------------------

boxplot(dadosMetrica3,
        col=(c("gold","darkgreen")),
        main="N�mero total de linhas de c�digo | Cyclomatic Complexity")
