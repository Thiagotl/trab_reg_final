
library(hnp) # pacote para envelope simulado
library(lmtest) # teste reset
library(car) # para teste de multicolinearidade (fatores de inflacao de variancia)
library(tseries)
library(tidyverse)

library(readr)
wine_data_red <- read_csv("winequality-red.csv")

View(wine_data_red)

glimpse(wine_data_red)
attach(wine_data_red)

str(wine_data_red)

cor(wine_data_red)

#plot(wine_data_red)

summary(wine_data_red)

fit<- lm(pH ~ `citric acid`+`residual sugar`+ 
            `fixed acidity` + `volatile acidity`, data = wine_data_red)
summary(fit)



summary(fit2)
step(fit)


##### Analise de influencia
n<-dim(wine_data_red)[1] # tamanho da amostra


# com a seguinte funcao se obtem varias medidas de influencia
influence.measures(fit)

# Alavancagem
hatvalues(fit)
h_bar<-fit$rank / n
limite<-2*h_bar
abline(plot(hatvalues(fit),ylab="Alavancagem"), 
       col="red", h=limite,lty=2)
which(hatvalues(fit)>limite)

# DFFIT
dffits(fit)
limite<-2*sqrt(fit$rank / n)
abline(plot(dffits(fit),ylab="DFFITS"), 
       col="red", h=c(-limite,limite),lty=2)
which(abs(dffits(fit))>limite)

# DFBETA
dfbetas(fit) # cada beta tem seu DF

dfb1<-dfbetas(fit)[,1]
dfb2<-dfbetas(fit)[,2]
dfb3<-dfbetas(fit)[,3]
dfb4<-dfbetas(fit)[,4]
dfb4<-dfbetas(fit)[,5]

limite<-2/sqrt(n)
abline(plot(dfb1,ylab="DFBETA 1"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))

abline(plot(dfb2,ylab="DFBETA 2"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))

# distancia de Cook
cooks.distance(fit)
limite<-4/(n-fit$rank )
abline(plot(cooks.distance(fit),ylab="Distancia de Cook"), 
       col="red", h=limite,lty=2)


# residuo
residuo <- rstudent(fit) # residuo studentizado

plot(residuo,type='p',pch="+",main="Residuos",xlab="indices") # plota os residuos do modelo
abline(h=c(-2,0,2),lty=3) # inclui linhas horizontais no grafico

which(abs(residuo)>3)

hist(residuo) # histograma dos residuos

# envelope simulado baseado nos residuos studentizados
hnp(fit,resid.type="student",halfnormal = F) # envelope simulado 



##### Testando as suposições do modelo

## [S0] O modelo estah corretamente especificado
## [S1] A media dos erros eh zero
## [s2] Homoscedasticidade dos erros
## [S3] Nao autocorrelacao 
## [S4] Ausencia de Multicolinearidade
## [S5] Normalidade dos erros

## Obs.: Para testes de hipoteses, se p-value < alpha (5%) 
## entao rejeita a hipotese nula (H0)

## Testa [S0]
## Teste RESET de especificacao
## H0: O modelo estah corretamente especificado
resettest(fit)

## Testa [S1]
## Teste t para a média dos errros
## H0: média dos erros eh igual a zero
t.test(resid(fit),mu=0,alternative="two.sided")

## Testa [s2]
## Teste de Bressch-Pagan (Koenker) de Heteroscedasticidade
## H0: erros sao homoscedasticos
bptest(fit, studentize = TRUE)

## Testa [S3]
## Teste de Durbin-Watson de autocorrelacao
## H0: : Nao hah autocorrelacao 
dwtest(fit)
acf(rstudent(fit))

## Testa [S4]
## Usa Fatores de Inflacao de Variancia para detectar multicolinearidade
## Regra de bolso: vif > 10 indica multicolinearidade. vif=1 seria o ideal.
vif(fit)

## Testa [S5]
## Teste Jarque-Bera de Normalidade
## H0: Os erros possuem distribuicao normal
jarque.bera.test(resid(fit))



step(fit) # seleciona modelo baseado no AIC e stepwise
# neste caso o modelo eh ok e o step nao excluiu covariaveis
step(fit2)
