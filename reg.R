library(hnp) # pacote para envelope simulado
library(lmtest) # teste reset
library(car) # para teste de multicolinearidade (fatores de inflacao de variancia)
library(tseries)
library(readr)

kc_house_data <- read_csv("kc_house_data.csv")
View(kc_house_data)

attach(kc_house_data)
str(kc_house_data)

# dados apenas com as variaveis de interesse

set.seed(123)
dados<-kc_house_data |> 
  dplyr::select(price, bedrooms, bathrooms, sqft_living, sqft_lot,
                floors, sqft_above, sqft_basement, yr_built, waterfront) |>  
  tidyr::drop_na()

dados_waterfront<-dados |> 
  dplyr::filter(waterfront == 1)
  
dados_waterfront0<-dados |> 
  dplyr::filter(waterfront == 0) |> 
  dplyr::sample_n(136, replace=TRUE)


banco_final<-rbind(dados_waterfront,dados_waterfront0)
#banco_final<-banco_final[1:150]


banco_final <- banco_final |> dplyr::mutate(id = dplyr::row_number())

# Índices dos outliers
outliers <- c(14,15,27,35,59,67,60,66,64,65,70,93,94,95,102,195,197,
              199,201,219,205,62,99,209,211,213,215)

# Remover as linhas correspondentes aos outliers
banco_final_novo<- banco_final |> dplyr::filter(!id %in% outliers) |> dplyr::select(-id)

# banco_final_novo<-banco_final |> 
#   dplyr::slice(-c(14,15,35,71,220))

View(banco_final_novo)
cor(banco_final)
plot(dados)

# first model
fit<-lm(price ~ . , data=dados_waterfront)

summary(fit)

step(fit)

fit2<-lm(formula = price ~ sqft_living + sqft_lot + floors + sqft_above + 
           waterfront, data = banco_final_novo)
summary(fit2)

##### Analise de influencia

n<-dim(banco_final)[1] # tamanho da amostra
n<-dim(banco_final_novo)[1]

# com a seguinte funcao se obtem varias medidas de influencia
influence.measures(fit2)

# Alavancagem
h_values<-hatvalues(fit2)
h_bar<-fit2$rank / n
limite<-2*h_bar
abline(plot(hatvalues(fit2),ylab="Alavancagem"), 
       col="red", h=limite,lty=2)
#which(hatvalues(fit2)>limite)

outliers <- which(h_values > limite)

text(outliers, h_values[outliers], labels=outliers, pos=4, col="blue", cex=0.8)

# DFFIT
dffits(fit2)
limite<-2*sqrt(fit2$rank / n)
abline(plot(dffits(fit2),ylab="DFFITS"), 
       col="red", h=c(-limite,limite),lty=2)
which(abs(dffits(fit2))>limite)

identify(dffits(fit2),n=5)


# DFBETA
dfbetas(fit) # cada beta tem seu DF

dfb1<-dfbetas(fit)[,1]
dfb2<-dfbetas(fit)[,2]
dfb3<-dfbetas(fit)[,3]
dfb4<-dfbetas(fit)[,4]

limite<-2/sqrt(n)
abline(plot(dfb1,ylab="DFBETA 1"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))

abline(plot(dfb2,ylab="DFBETA 2"), 
       col=c("red","blue","red"), h=c(-limite,0,limite),lty=c(2,1,2))

# distancia de Cook
cooks.distance(fit2)
limite<-4/(n-fit2$rank)
abline(plot(cooks.distance(fit2),ylab="Distancia de Cook"), 
       col="red", h=limite,lty=2)

identify(cooks.distance(fit2), n=2)

# residuo
residuo <- rstudent(fit2) # residuo studentizado

plot(residuo,type='p',pch="+",main="Residuos",xlab="indices") # plota os residuos do modelo
abline(h=c(-2,0,2),lty=3) # inclui linhas horizontais no grafico

outliers_high <- which(residuo > 3)
outliers_low <- which(residuo < -3)

# Adiciona pontos em vermelho para os resíduos maiores que 3
points(outliers_high, residuo[outliers_high], col="red", pch=19)

# Adiciona pontos em azul para os resíduos menores que -3
points(outliers_low, residuo[outliers_low], col="blue", pch=19)

# Adiciona rótulos para os outliers maiores que 3
text(outliers_high, residuo[outliers_high], labels=outliers_high, pos=4, col="red", cex=0.8)

# Adiciona rótulos para os outliers menores que -3
text(outliers_low, residuo[outliers_low], labels=outliers_low, pos=4, col="blue", cex=0.8)


which(abs(residuo)>3)

identify(rstudent(fit), n=5)

hist(residuo) # histograma dos residuos

# envelope simulado baseado nos residuos studentizados
hnp(fit2,resid.type="student",halfnormal = F) # envelope simulado 



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
resettest(fit2)

## Testa [S1]
## Teste t para a média dos errros
## H0: média dos erros eh igual a zero
t.test(resid(fit2),mu=0,alternative="two.sided")

## Testa [s2]
## Teste de Bressch-Pagan (Koenker) de Heteroscedasticidade
## H0: erros sao homoscedasticos
bptest(fit2, studentize = TRUE)

## Testa [S3]
## Teste de Durbin-Watson de autocorrelacao
## H0: : Nao hah autocorrelacao 
dwtest(fit2)
acf(rstudent(fit2))

## Testa [S4]
## Usa Fatores de Inflacao de Variancia para detectar multicolinearidade
## Regra de bolso: vif > 10 indica multicolinearidade. vif=1 seria o ideal.
vif(fit2)

## Testa [S5]
## Teste Jarque-Bera de Normalidade
## H0: Os erros possuem distribuicao normal
jarque.bera.test(resid(fit2))

step(fit) # seleciona modelo baseado no AIC e stepwise
# neste caso o modelo eh ok e o step nao excluiu covariaveis
step(fit)
