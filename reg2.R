library(hnp) # pacote para envelope simulado
library(lmtest) # teste reset
library(car) # para teste de multicolinearidade (fatores de inflacao de variancia)
library(tseries)
library(readr)




kc_house_data <- read_csv("kc_house_data.csv")
View(kc_house_data)

attach(kc_house_data)
str(kc_house_data)

banco_final <- kc_house_data|> 
  dplyr::select(price, bathrooms, bedrooms, floors, sqft_living, 
                sqft_above, sqft_lot, sqft_basement, yr_built, waterfront) |> 
  tidyr::drop_na()


View(banco_final)
banco_final<-banco_final[1:110,]

banco_final <- banco_final |> dplyr::mutate(id = dplyr::row_number())

# banco_final_novo<-banco_final |> 
#   dplyr::slice(-c(20,47,67,123,144))


# Índices dos outliers
outliers <- c(20,61,63,64,66,67)

banco_final_novo<- banco_final |> dplyr::filter(!id %in% outliers) |> dplyr::select(-id)


fit<-lm(price~., data=banco_final_novo)
summary(fit)
step(fit)


# fit2<-lm(formula = price ~ sqft_living + sqft_lot + yr_built + waterfront, 
#          data = banco_final_novo)
# summary(fit2)


fit3<-lm(formula = price ~ floors + sqft_living + yr_built + waterfront, 
         data = banco_final_novo)


summary(fit3)
#tamanho do banco

n<-dim(banco_final)[1]
n<-dim(banco_final_novo)[1]

# com a seguinte funcao se obtem varias medidas de influencia
influence.measures(fit3)

# Alavancagem
h_values<-hatvalues(fit3)
h_bar<-fit3$rank / n
limite<-2*h_bar
abline(plot(hatvalues(fit3),ylab="Alavancagem"), 
       col="red", h=limite,lty=2)
#which(hatvalues(fit2)>limite)

outliers <- which(h_values > limite)

text(outliers, h_values[outliers], labels=outliers, pos=4, col="blue", cex=0.8)

# distancia de Cook
cooks.distance(fit3)
limite<-4/(n-fit3$rank)
abline(plot(cooks.distance(fit3),ylab="Distancia de Cook"), 
       col="red", h=limite,lty=2)

#identify(cooks.distance(fit2), n=2)

# Identificar os pontos que são maiores que o limite
outliers_cook <- which(cooks.distance(fit3) > limite)

# Adiciona pontos em destaque para os valores acima do limite
points(outliers_cook, cooks.distance(fit3)[outliers_cook], col="blue", pch=19)

# Adiciona rótulos para os pontos acima do limite
text(outliers_cook, cooks.distance(fit3)[outliers_cook], labels=outliers_cook, pos=4, col="blue", cex=0.8)



 


which(abs(residuo)>3)

#identify(rstudent(fit), n=5)

hist(residuo) # histograma dos residuos

# envelope simulado baseado nos residuos studentizados
hnp(fit3,resid.type="student",halfnormal = F) # envelope simulado 


## Testa [S0]
## Teste RESET de especificacao
## H0: O modelo estah corretamente especificado
resettest(fit3)

## Testa [S1]
## Teste t para a média dos errros
## H0: média dos erros eh igual a zero
t.test(resid(fit3),mu=0,alternative="two.sided")

## Testa [s2]
## Teste de Bressch-Pagan (Koenker) de Heteroscedasticidade
## H0: erros sao homoscedasticos
bptest(fit3, studentize = TRUE)

## Testa [S3]
## Teste de Durbin-Watson de autocorrelacao
## H0: : Nao hah autocorrelacao 
dwtest(fit3)
acf(rstudent(fit3))

## Testa [S4]
## Usa Fatores de Inflacao de Variancia para detectar multicolinearidade
## Regra de bolso: vif > 10 indica multicolinearidade. vif=1 seria o ideal.
vif(fit3)

## Testa [S5]
## Teste Jarque-Bera de Normalidade
## H0: Os erros possuem distribuicao normal
jarque.bera.test(resid(fit3))
